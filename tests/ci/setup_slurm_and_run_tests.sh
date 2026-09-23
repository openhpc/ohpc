#!/bin/bash

set -x
set -e

# Dispatch which resource manager to set up and test based on which
# name this script is invoked as; setup_flux_and_run_tests.sh is a
# symlink to this file (see below), so RMS can only be told apart by
# argv[0], not by content.
case "$(basename "$0")" in
setup_flux_and_run_tests.sh)
	RMS=flux
	;;
setup_slurm_and_run_tests.sh)
	RMS=slurm
	;;
*)
	echo "Unknown resource manager for invocation name '$0'" >&2
	exit 1
	;;
esac

USER=$1
shift

COMPILER_FAMILY=$1
shift

PKG=("dnf" "-y")

if hash zypper >/dev/null 2>&1; then
	PKG=("zypper" "-n" "--no-gpg-checks")
fi

install_packages() {
	# First remove a possible conflicts from a previous run
	"${PKG[@]}" remove lmod-defaults-*-ohpc || true

	local rms_pkgs=()
	if [ "${RMS}" = "flux" ]; then
		rms_pkgs=(flux-security-ohpc flux-core-ohpc flux-sched-ohpc flux-pmix-ohpc)
		rms_pkgs=()
	else
		rms_pkgs=(slurm-slurmd-ohpc slurm-slurmctld-ohpc slurm-example-configs-ohpc slurm-ohpc)
	fi

	# Then install the resource manager and needed packages
	local install_pkgs=(
		hostname
		make
		openssh-clients
		which
		sudo
		psmisc
		autoconf
		automake
		libtool
		prun-ohpc
		openmpi5-"${COMPILER_FAMILY}"-ohpc
		mpich-"${COMPILER_FAMILY}"-ohpc
		lmod-defaults-"${COMPILER_FAMILY}"-openmpi5-ohpc
		"${rms_pkgs[@]}"
	)

	if [ "$(uname -m)" != "aarch64" ]; then
		install_pkgs+=(mvapich2-"${COMPILER_FAMILY}"-ohpc)
	fi

	"${PKG[@]}" install "${install_pkgs[@]}"

	# Install rebuilt packages (if any)
	local find_exclude=(! -name "*arm1*")
	if [ "${COMPILER_FAMILY}" != "intel" ]; then
		find_exclude+=(! -name "*-intel-*")
	fi
	# shellcheck disable=SC2046 # (we want the words to be split)
	"${PKG[@]}" install $(find /home/"${USER}"/rpmbuild/RPMS/ -name "*rpm" "${find_exclude[@]}") || true

	# Flux and Slurm are alternatives, not co-installed. Remove OpenPBS,
	# and, for flux, also Slurm, if either is present as a dependency of
	# something else.
	"${PKG[@]}" remove openpbs-*-ohpc || true
	if [ "${RMS}" = "flux" ]; then
		"${PKG[@]}" remove slurm-*-ohpc || true
	fi
	rm -f /etc/pbs.conf
}

# Common to both RMS: bring up munged, the shared auth daemon both
# slurmctld/slurmd and the flux system instance need. Slurm gets its
# munge key from slurm-example-configs-ohpc's own %post; flux installs
# no such package, so generate one directly here instead.
setup_munge() {
	chown root.root /var/log/munge

	mkdir -p /run/munge

	if [ "${RMS}" = "flux" ] && [ ! -e /etc/munge/munge.key ] && [ -c /dev/urandom ]; then
		/bin/dd if=/dev/urandom bs=1 count=1024 >/etc/munge/munge.key 2>/dev/null
		/bin/chown munge:munge /etc/munge/munge.key
		/bin/chmod 0400 /etc/munge/munge.key
	fi

	if [ "${RMS}" = "flux" ]; then
		killall munged || true
	else
		killall munged slurmctld slurmd || true
	fi

	/usr/sbin/munged -f
}

setup_slurm() {
	echo "127.0.0.1 node0 node1" >>/etc/hosts

	cp /etc/slurm/slurm.conf.example /etc/slurm/slurm.conf

	sed -i -e "
		s,SlurmdLogFile=.*$,SlurmdLogFile=/var/log/slurmd.%n.log,g; \
		s,SlurmdSpoolDir=.*$,SlurmdSpoolDir=/var/spool/slurmd.%n,g; \
		s,SlurmdPidFile=.*$,SlurmdPidFile=/var/run/slurmd.%n.pid,g; \
		s,JobCompType=jobcomp/none,,g; \
		s,ProctrackType=.*,ProctrackType=proctrack/linuxproc,g; \
		s,TaskPlugin=.*,TaskPlugin=task/none,g; \
		s,NodeName=.*$,,g; \
		s,PartitionName.*$,,g; \
		s,ReturnToService.*$,ReturnToService=2,g; \
		s,SlurmctldHost=.*$,SlurmctldHost=${HOSTNAME},g;" /etc/slurm/slurm.conf

	{
		echo "NodeName=c0 NodeHostname=node0 Port=17004 CPUs=2"
		echo "NodeName=c1 NodeHostname=node1 Port=17005 CPUs=2"
		echo "PartitionName=normal Nodes=c0,c1 Default=YES MaxTime=24:00:00 State=UP"
	} >>/etc/slurm/slurm.conf

	# cgroupv2 support does not yet work in containers.
	# Force cgroupv1 even on hosts with v2.
	echo "CgroupPlugin=cgroup/v1" >/etc/slurm/cgroup.conf

	setup_munge

	/usr/sbin/slurmctld
	slurmd -N c0 --conf-server localhost || cat /var/log/slurm*
	slurmd -N c1 --conf-server localhost || cat /var/log/slurm*

	sinfo

	local retry_counter=0
	local max_retries=5

	while true; do
		((retry_counter += 1))
		if [ "${retry_counter}" -gt "${max_retries}" ]; then
			exit 1
		fi
		scontrol update nodename=c[0-1] state=idle && break
		sinfo

		# In case it is a network error let's wait a bit.
		echo "Retrying scontrol attempt ${retry_counter}"
		sleep "${retry_counter}"
	done

	srun -N2 hostname
}

# Set up a real, persistent, multi-user flux system instance, the same
# role slurmctld/slurmd play in setup_slurm() above, rather than a
# throwaway personal "flux start --test-size=N" instance: this is what
# lets the actual tests/ harness run against flux the same way it runs
# against slurm. flux-core has no per-node daemon or port scheme the
# way slurmd does, so two real "flux broker" processes are started
# directly here, each given a distinct simulated hostname (node0/node1)
# and a distinct local TCP port, bootstrapped from a static config
# rather than PMI (see flux-config-bootstrap(5); PMI bootstrap is for a
# job launching a Flux subinstance, not for the system instance
# itself).
#
# flux-core determines a broker's own rank by matching gethostname()
# against the "hosts" list in that config, which -Shostname overrides
# per broker process since both actually run on the same host.
setup_flux() {
	setup_munge

	# A CURVE certificate is required for instance sizes > 1 and must be
	# owned by the instance owner (flux); generate it as flux directly
	# into a location it can already write, then place it.
	sudo --user=flux flux keygen /tmp/flux-curve.cert
	mv /tmp/flux-curve.cert /etc/flux/system/curve.cert
	chown flux:flux /etc/flux/system/curve.cert
	chmod 600 /etc/flux/system/curve.cert

	cat >/etc/flux/system/conf.d/bootstrap.toml <<'EOF'
[bootstrap]
curve_cert = "/etc/flux/system/curve.cert"

hosts = [
	{ host = "node0", bind = "tcp://127.0.0.1:8050", connect = "tcp://127.0.0.1:8050" },
	{ host = "node1", connect = "tcp://127.0.0.1:8050" },
]
EOF

	# The system instance denies all non-owner access by default; allow
	# guest users like the one that runs the actual test suite below to
	# connect at all.
	cat >/etc/flux/system/conf.d/access.toml <<'EOF'
[access]
allow-guest-user = true
EOF

	# ...and tell flux-core where to find flux-security's IMP, without
	# which guest jobs fail immediately with "failed to initialize
	# implementation" (see flux-config-exec(5): exec.imp; unset, only the
	# instance owner's own jobs may run).
	cat >/etc/flux/system/conf.d/exec.toml <<'EOF'
[exec]
imp = "/usr/libexec/flux/flux-imp"
EOF

	# flux-security's own IMP configuration: which user may invoke
	# "flux-imp exec" (the flux system instance itself, not the guest
	# being exec'd as) and which job shell it is allowed to run as that
	# guest (see flux-config-security-imp(5)). Root-owned and
	# world-unwritable, as the IMP itself requires.
	cat >/etc/flux/imp/conf.d/exec.toml <<'EOF'
[exec]
allowed-users = ["flux"]
allowed-shells = ["/usr/libexec/flux/flux-shell"]
EOF
	chown root:root /etc/flux/imp/conf.d/exec.toml
	chmod 644 /etc/flux/imp/conf.d/exec.toml

	# %pre already creates the flux user with a real shell rather than
	# nologin (nologin breaks rc1's "flux modprobe rc1" step), but fix it
	# here too in case this container already had the user from an older
	# package build, since useradd only runs on the user's first install.
	usermod -s /bin/bash flux

	# The broker's own argv[0] is literally "broker", not "flux-broker"
	# ("flux" execs it directly); "killall flux-broker" silently matches
	# nothing, leaving old broker(s) from a prior run of this script still
	# bound to the node0 TCP port and making this run fail with "Address
	# already in use". Match on something specific to how this script
	# itself invokes it instead.
	#
	# -9: a plain (SIGTERM) pkill here leaves the old broker(s) running
	# for several more seconds while they run their own graceful shutdown
	# sequence (rc3, module unloads, etc, visible in their log), well past
	# a short fixed sleep; nothing about that shutdown matters for a stale
	# process from a prior run, so kill it outright instead of waiting for
	# it to be graceful with itself.
	pkill -9 -f 'broker --config-path=/etc/flux/system/conf.d' 2>/dev/null || true
	sleep 2

	local node
	for node in node0 node1; do
		rm -rf "/run/flux-${node}" "/var/lib/flux-${node}"
		mkdir -p "/run/flux-${node}" "/var/lib/flux-${node}"
		chown flux:flux "/run/flux-${node}" "/var/lib/flux-${node}"

		# A leftover log from a prior run is root-owned (the broker's
		# own redirect used to be opened by this root shell); remove it
		# so flux, unprivileged, can create a fresh one instead of
		# failing to open the existing one for writing.
		rm -f "/tmp/flux-${node}.log"
	done

	for node in node0 node1; do
		# A plain "sudo ... > file" would have this still-root shell
		# open the redirect before sudo drops to flux, which is exactly
		# what triggers shellcheck's SC2024. Run the whole thing,
		# redirect included, inside the sudo'd shell instead, so flux
		# itself opens the log (world-writable /tmp, no privilege
		# needed either way).
		sudo --user=flux bash -c "
			flux broker \
				--config-path=/etc/flux/system/conf.d \
				-Shostname='${node}' \
				-Srundir='/run/flux-${node}' \
				-Sstatedir='/var/lib/flux-${node}' \
				-Slocal-uri='local:///run/flux-${node}/local' \
				-Sbroker.rc2_none \
				>'/tmp/flux-${node}.log' 2>&1
		" &
		sleep 1
	done

	export FLUX_URI=local:///run/flux-node0/local

	local retry_counter=0
	local max_retries=5

	while true; do
		((retry_counter += 1))
		if [ "${retry_counter}" -gt "${max_retries}" ]; then
			cat /tmp/flux-node0.log /tmp/flux-node1.log
			exit 1
		fi
		sudo --user=flux --preserve-env=FLUX_URI flux overlay status | grep -q "node1: full" && break

		# In case it is a network error let's wait a bit.
		echo "Retrying flux overlay status attempt ${retry_counter}"
		sleep "${retry_counter}"
	done

	sudo --user=flux --preserve-env=FLUX_URI flux overlay status

	sudo --user="${USER}" --preserve-env=FLUX_URI flux run -N2 hostname
}

run_test_suite() {
	# Figure out which tests we need to run.
	# This script returns three array variables:
	#  PKGS and TESTS and ADMIN_TESTS
	# shellcheck disable=SC2068 # (we want individual elements)
	eval "$(tests/ci/spec_to_test_mapping.py --compiler-family "${COMPILER_FAMILY}" $@)"

	if [ "${#PKGS[@]}" -gt 0 ]; then
		"${PKG[@]}" install "${PKGS[@]}"
	fi

	export SIMPLE_CI=1
	TESTS_FAILED=1

	export OHPC_USE_CCACHE=yes
	chown -R ohpc:ohpc /var/cache/ccache

	set +e

	# AppArmor on the Ubuntu GitHub Actions host might block
	# access to /etc/shadow.
	chmod 644 /etc/shadow

	sudo --user="${USER}" --login bash -c "cd ${PWD}/tests; find ./ -name '*.log' -delete"

	# Always running at least with '--enable-modules'. No need to check for
	# an empty TESTS array.
	local MPI_FAMILIES="openmpi5 mpich"

	## Disabling this for now. Sometimes mvapich2 based tests are not working in GitHub Actions.
	##if [ "$(uname -m)" != "aarch64" ]; then
	##	MPI_FAMILIES="${MPI_FAMILIES} mvapich2"
	##fi

	local preserve_flux_uri=()
	if [ "${RMS}" = "flux" ]; then
		preserve_flux_uri=(--preserve-env=FLUX_URI)
	fi

	if sudo \
		--user="${USER}" \
		--preserve-env=SIMPLE_CI \
		--preserve-env=OHPC_USE_CCACHE \
		"${preserve_flux_uri[@]}" \
		--login \
		bash -c "\
			cd ${PWD}/tests; \
			./bootstrap; \
			./configure \
				--disable-all \
				--enable-modules \
				--enable-rms-harness \
				--enable-compilers \
				--with-compiler-families='${COMPILER_FAMILY}' \
				--with-mpi-families='${MPI_FAMILIES}' \
				${TESTS[*]}; \
			make check"; then
		TESTS_FAILED=0
	fi

	if [ "${#ADMIN_TESTS[@]}" -gt 0 ]; then
		# The configure script uses the variable $USER to decide if root or not
		export USER=root
		cd tests
		./bootstrap
		./configure --disable-all --disable-bos --disable-oob --disable-spack "${ADMIN_TESTS[*]}"
		if ! make check; then
			TESTS_FAILED=1
		fi
	fi

	if [ "${TESTS_FAILED}" -eq 0 ]; then
		cd tests
		make distclean >/dev/null 2>&1 || true
		exit 0
	fi

	set -e

	# If we are here, the tests failed. Print the logs and exit with an error code.
	echo -e "\nThe tests execution failed. Printing the logs.\n"
	find ./ -name "*.log" -print0 | while IFS= read -r -d '' log_file; do
		echo "================================================"
		echo "Log file: ${log_file}"
		cat "${log_file}"
	done
	exit 1
}

install_packages

if [ "${RMS}" = "flux" ]; then
	setup_flux
else
	setup_slurm
fi

run_test_suite "$@"
