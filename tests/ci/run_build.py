#!/usr/bin/env python3

import subprocess
import selectors
import argparse
import logging
import shutil
import time
import pwd
import sys
import csv
import os
import io

logging.basicConfig(format="%(asctime)s %(message)s", level=logging.INFO)

parser = argparse.ArgumentParser()
parser.add_argument(
    "user",
    help="non root user to run build as",
    nargs=1,
)
parser.add_argument(
    "specfiles",
    help="list of specfiles to check",
    nargs="*",
)
parser.add_argument(
    "--compiler-family",
    help="compiler family name to use for rebuild",
    default="gnu15",
)
parser.add_argument(
    "--mpi-family",
    help=(
        "mpi family name to use for rebuild "
        + "(defaults to openmpi5, mpich, mvapich2)"
    ),
)
args = parser.parse_args()

spec_found = False
build_user = "".join(args.user)
dnf_based = False
dist = "9999.ci.ohpc"
version_id = ""

# Check which base OS we are using
reader = csv.DictReader(open("/etc/os-release"), delimiter="=")

skip_ci_specs = []
skip_ci_specs_env = os.getenv("SKIP_CI_SPECS")
if skip_ci_specs_env:
    skip_ci_specs = skip_ci_specs_env.rstrip().split()

for row in reader:
    key = row.pop("NAME")
    if key in ["ID_LIKE", "ID"]:
        for item in list(row.items())[0]:
            if "fedora" in item or "openEuler" in item:
                dnf_based = True
    if key == "VERSION_ID":
        version_id = list(row.items())[0][1]

# Enable ccache for CI builds
os.makedirs("/etc/rpm", exist_ok=True)
with open("/etc/rpm/macros.ohpc-ci-conf", "w") as f:
    f.write("%OHPC_USE_CCACHE yes\n")

# Ensure ccache directory is owned by build user
ccache_dir = "/var/cache/ccache"
os.makedirs(ccache_dir, exist_ok=True)
uid = pwd.getpwnam(build_user).pw_uid
gid = pwd.getpwnam(build_user).pw_gid
for root, dirs, files in os.walk(ccache_dir):
    os.chown(root, uid, gid)
    for d in dirs:
        os.chown(os.path.join(root, d), uid, gid)
    for f in files:
        os.chown(os.path.join(root, f), uid, gid)

rpmbuild_rpms_dir = os.path.join(pwd.getpwnam(build_user).pw_dir, "rpmbuild", "RPMS")
local_repo_configured = False


def run_command(command):
    logging.info("About to run command %s" % " ".join(command))
    process = subprocess.Popen(
        command,
        bufsize=1,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        universal_newlines=True,
    )

    buf = io.StringIO()

    def handle_output(stream, mask):
        line = stream.readline()
        buf.write(line)
        sys.stdout.write(line)

    selector = selectors.DefaultSelector()
    selector.register(
        process.stdout,
        selectors.EVENT_READ,
        handle_output,
    )

    while process.poll() is None:
        events = selector.select()
        for key, mask in events:
            callback = key.data
            callback(key.fileobj, mask)

    return_code = process.wait()
    selector.close()

    output = buf.getvalue()
    buf.close()

    return ((return_code == 0), output)


def loop_command(command, max_attempts=5):
    output = None
    attempt_counter = 0

    while True:
        attempt_counter += 1

        try:
            (success, output) = run_command(command)
            if success:
                return (True, output)
        except Exception as e:
            logging.error("Exception: %s" % e)

        if attempt_counter >= abs(max_attempts):
            return (False, output)

        logging.info("Retrying attempt '%i'" % attempt_counter)
        time.sleep(attempt_counter)


def get_build_order(specfiles):
    """Use misc/build_order.sh to determine the correct build order
    for the given spec files."""
    logging.info("Determining build order using misc/build_order.sh")

    # Ensure RPM build environment is set up (build_order.sh requires
    # OHPC_macros in the RPM source directory)
    sourcedir = subprocess.check_output(
        ["rpm", "--eval", "%{_sourcedir}"], text=True
    ).strip()
    topdir = subprocess.check_output(["rpm", "--eval", "%{_topdir}"], text=True).strip()
    for subdir in ["BUILD", "BUILDROOT", "RPMS", "SOURCES", "SPECS", "SRPMS"]:
        os.makedirs(os.path.join(topdir, subdir), exist_ok=True)
    macros_dest = os.path.join(sourcedir, "OHPC_macros")
    if not os.path.exists(macros_dest):
        macros_src = "components/OHPC_macros"
        if os.path.exists(macros_src):
            shutil.copy2(macros_src, macros_dest)
            logging.info("Copied OHPC_macros to %s" % macros_dest)
        else:
            logging.warning("components/OHPC_macros not found")

    success, output = run_command(["misc/build_order.sh"])
    if not success:
        logging.warning("misc/build_order.sh failed, using original spec file order")
        return specfiles

    # build_order.sh prints all spec paths in order on one line
    ordered = output.strip().split()

    # Normalize paths: strip leading "./" for consistent comparison
    def normalize(p):
        while p.startswith("./"):
            p = p[2:]
        return p

    ordered_normalized = [normalize(p) for p in ordered]

    # Build a position map from the full build order
    position = {}
    for idx, path in enumerate(ordered_normalized):
        position[path] = idx
        # Also index by basename for fallback matching
        position[os.path.basename(path)] = idx

    def sort_key(spec_path):
        norm = normalize(spec_path)
        if norm in position:
            return position[norm]
        base = os.path.basename(norm)
        if base in position:
            return position[base]
        # Specs not found in build order go to the end
        return len(ordered)

    sorted_specs = sorted(specfiles, key=sort_key)
    logging.info(
        "Build order: %s" % " ".join([os.path.basename(s) for s in sorted_specs])
    )
    return sorted_specs


def setup_local_repo():
    """Run createrepo_c on the rpmbuild RPMS directory and configure
    it as a local repository so that subsequent builds can use
    previously built RPMs as dependencies."""
    global local_repo_configured

    os.makedirs(rpmbuild_rpms_dir, exist_ok=True)

    logging.info("Running createrepo_c on %s" % rpmbuild_rpms_dir)
    success, _ = run_command(["createrepo_c", rpmbuild_rpms_dir])
    if not success:
        logging.error("createrepo_c failed on %s" % rpmbuild_rpms_dir)
        return False

    if not local_repo_configured:
        if dnf_based:
            repo_file = "/etc/yum.repos.d/local-ohpc-ci.repo"
            with open(repo_file, "w") as f:
                f.write("[local-ohpc-ci]\n")
                f.write("name=Local OpenHPC CI builds\n")
                f.write("baseurl=file://%s\n" % rpmbuild_rpms_dir)
                f.write("enabled=1\n")
                f.write("gpgcheck=0\n")
            logging.info("Configured local DNF repository at %s" % repo_file)
        else:
            success, _ = run_command(
                [
                    "zypper",
                    "ar",
                    "-G",
                    "-f",
                    rpmbuild_rpms_dir,
                    "local-ohpc-ci",
                ]
            )
            if not success:
                logging.error("Failed to add local zypper repository")
                return False
            logging.info("Configured local zypper repository")

        local_repo_configured = True

    return True


def build_srpm_and_rpm(
    command, mpi_family=None, compiler_family=None, not_mpi_dependent=False
):
    # Build SRPM
    command = [
        "misc/build_srpm.sh",
        spec,
    ]
    if compiler_family:
        command.append(compiler_family)
    if compiler_family and mpi_family:
        command.append(mpi_family)
    if not_mpi_dependent:
        if not mpi_family:
            # This is a shortcoming of the build_srpm script.
            # It only has positional parameters.
            # It needs a dummy parameter here.
            command.append("openmpi5")
        command.append("0")
    success, output = run_command(command)
    if not success:
        # First check if the architecture is not supported
        if output is not None:
            for line in output.split("\n"):
                if "No compatible architectures found for build" in line:
                    logging.info("Skipping unsupported architecture RPM")
                    return True

        logging.error("Running misc/build_srpm.sh failed")
        return False

    src_rpm = ""
    for line in output.split("\n"):
        if line.endswith(".src.rpm"):
            src_rpm = line
            break

    if src_rpm == "":
        logging.error("SRPM generation failed")
        return False

    logging.info(src_rpm)

    if dnf_based:
        builddep_command = [
            "dnf",
            "-y",
            "builddep",
            src_rpm,
        ]
    else:
        builddep_command = [
            "zypper",
            "-n",
            "--no-gpg-checks",
            "source-install",
            src_rpm,
        ]

    success, _ = loop_command(builddep_command)
    if not success:
        logging.error("Running '%s' failed" % " ".join(builddep_command))
        return False

    tmp_src_rpm = os.path.join("/tmp", os.path.basename(src_rpm))

    try:
        os.unlink(tmp_src_rpm)
    except FileNotFoundError:
        pass
    shutil.move(src_rpm, "/tmp/")
    src_rpm = tmp_src_rpm

    rebuild_command = [
        "su",
        build_user,
        "-l",
        "-c",
        'rpmbuild --define "dist %s" --rebuild %s' % (dist, src_rpm),
    ]

    if mpi_family is not None:
        rebuild_command[-1] += " --define 'mpi_family %s'" % mpi_family

    if compiler_family is not None:
        rebuild_command[-1] += " --define 'compiler_family %s'" % compiler_family

    if not_mpi_dependent:
        rebuild_command[-1] += " --define 'ohpc_mpi_dependent 0'"

    # Disable parallel builds for below packages on aarch64 to avoid OOM
    pkgs = ["boost-", "paraver-"]
    if any([x in src_rpm for x in pkgs]) and os.uname().machine == "aarch64":
        rebuild_command[-1] += " --define '_smp_mflags -j1'"

    logging.warning(rebuild_command)
    success, _ = run_command(rebuild_command)
    if not success:
        logging.error("Running 'rpmbuild --rebuild' failed")
        return False

    return True


skipped = []
failed = []
rebuild_success = []
total = 0
docs_spec_executed = False

# Determine the number of actual spec files to build
specfiles = args.specfiles
spec_count = sum(
    1
    for s in specfiles
    if s.endswith(".spec")
    or "components/admin/docs/SPECS/docs.spec" == s
    or "docs/recipes/install/" in s
    or "components/admin/docs/SOURCES/" in s
)
multiple_specs = spec_count > 1
build_order_used = None

if multiple_specs:
    specfiles = get_build_order(specfiles)
    build_order_used = [os.path.basename(s) for s in specfiles if s.endswith(".spec")]

for spec in specfiles:
    # if more than one docs related file are modified then
    # build the docs.spec just once
    # START OF LOGIC FOR DOCS
    if "components/admin/docs/SPECS/docs.spec" == spec:
        if docs_spec_executed:
            continue
        docs_spec_executed = True
    elif not docs_spec_executed and (
        "docs/recipes/install/" in spec or "components/admin/docs/SOURCES/" in spec
    ):
        docs_spec_executed = True
        spec = "components/admin/docs/SPECS/docs.spec"
    # END OF LOGIC FOR DOCS
    elif not spec.endswith(".spec"):
        continue
    just_spec = os.path.basename(spec)
    total += 1
    if spec in skip_ci_specs:
        logging.info("--> Skipping spec file %s" % spec)
        skipped.append(just_spec)
        continue

    spec_found = True
    logging.info("--> Building RPM from spec file %s" % spec)

    command = [
        "misc/get_source.sh",
        just_spec,
    ]

    success, _ = run_command(command)
    if not success:
        logging.error("Running misc/get_source.sh failed")
        failed.append(just_spec)
        continue

    # cache spec file contents
    infile = open(spec)
    contents = infile.read()
    infile.close()

    if "ohpc_mpi_dependent" in contents:
        families = [
            "openmpi5",
            "mpich",
            "mvapich2",
        ]

        if args.mpi_family:
            families = [args.mpi_family]

        for family in families:
            if family == "mvapich2" and os.uname().machine == "aarch64":
                continue
            if not build_srpm_and_rpm(
                spec,
                mpi_family=family,
                compiler_family=args.compiler_family,
            ):
                failed.append("%s (%s, %s)" % (just_spec, args.compiler_family, family))
            else:
                rebuild_success.append(
                    "%s (%s, %s)" % (just_spec, args.compiler_family, family)
                )

        if "!?ohpc_mpi_dependent" in contents:
            # This is a package that can be built with and without MPI support.
            # It should have the following line:
            # '%{!?ohpc_mpi_dependent:%define ohpc_mpi_dependent 1}'
            # If that exists we need to rebuild it once more with
            # ohpc_mpi_dependent set to 0.
            if not build_srpm_and_rpm(
                spec,
                compiler_family=args.compiler_family,
                not_mpi_dependent=True,
            ):
                failed.append("%s (%s)" % (just_spec, args.compiler_family))
            else:
                rebuild_success.append("%s (%s)" % (just_spec, args.compiler_family))

    elif "ohpc_compiler_dependent" in contents:
        if not build_srpm_and_rpm(
            spec,
            compiler_family=args.compiler_family,
        ):
            failed.append("%s (%s)" % (just_spec, args.compiler_family))
        else:
            rebuild_success.append("%s (%s)" % (just_spec, args.compiler_family))

    else:
        if not build_srpm_and_rpm(spec):
            failed.append(just_spec)
        else:
            rebuild_success.append(just_spec)

    # When building multiple specs, update the local repo after each
    # spec so that subsequent builds can use the just-built RPMs
    # as dependencies.
    if multiple_specs:
        setup_local_repo()


if not spec_found:
    logging.info("SKIP. Commit without changes to a SPEC file.")

logging.info("Found %d spec file(s)" % total)
if build_order_used:
    logging.info("--> Build order: %s" % " -> ".join(build_order_used))
logging.info("--> %d rebuild successfully" % len(rebuild_success))
for success in rebuild_success:
    logging.info("----> %s" % success)

logging.info("--> %d rebuilds skipped" % len(skipped))

for skip in skipped:
    logging.info("----> %s skipped" % skip)

if len(failed) > 0:
    logging.error("--> %d rebuilds failed" % len(failed))
    for fail in failed:
        logging.info("----> %s failed" % fail)

    logging.error("ERROR")
    sys.exit(1)

logging.info("No errors found. Exiting.")
sys.exit(0)
