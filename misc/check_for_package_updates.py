#!/usr/bin/env python3

"""Check for newer releases of OpenHPC packages by analyzing spec files.

Supports GitHub, GitLab, PyPI, JSC perftools, UOregon TAU/PDT,
BSC tools FTP, PnetCDF, Open MPI, GNU FTP, and
release-monitoring.org (Anitya) as a fallback.
"""

import argparse
import glob
import json
import os
import re
import shutil
import subprocess
import sys
import textwrap
import time

import requests

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Colors for terminal output
RED = "\033[0;31m"
GREEN = "\033[0;32m"
YELLOW = "\033[1;33m"
BLUE = "\033[0;34m"
NC = "\033[0m"

# Status constants
UPDATE_AVAILABLE = "UPDATE_AVAILABLE"
UP_TO_DATE = "UP_TO_DATE"
AHEAD = "AHEAD"
ERROR = "ERROR"
RATE_LIMITED = "RATE_LIMITED"
NOT_FOUND = "NOT_FOUND"
UNKNOWN = "UNKNOWN"

# Map from upstream package names (after stripping -ohpc, -gnu*-ohpc,
# -gnu*-openmpi*-ohpc suffixes) to release-monitoring.org project names
# or project IDs.  Use "id:<number>" for project IDs to disambiguate
# packages with duplicate names.  Only needed when names differ or are
# ambiguous; packages with unique matching names need no entry.
RELEASE_MONITORING_MAP = {
    "R": "id:386062",
    "omb": "osu-micro-benchmarks",
    "valgrind": "id:13639",
    "mpich": "id:8071",
    "gsl": "id:1267",
    "cmake": "id:306",
}


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class RateLimitError(Exception):
    """Raised when the GitHub API rate limit is exceeded."""


# ---------------------------------------------------------------------------
# Result container
# ---------------------------------------------------------------------------


class Result:
    """Container for a single package-check result."""

    __slots__ = (
        "name",
        "current_version",
        "latest_version",
        "status",
        "repo",
        "spec_file",
    )

    def __init__(
        self, name, current_version, latest_version, status, repo, spec_file=None
    ):
        self.name = name
        self.current_version = current_version
        self.latest_version = latest_version
        self.status = status
        self.repo = repo
        self.spec_file = spec_file


# ---------------------------------------------------------------------------
# Logging helpers
# ---------------------------------------------------------------------------


def log_error(msg):
    print(f"{RED}[ERROR]{NC} {msg}", file=sys.stderr)


def log_warn(msg):
    print(f"{YELLOW}[WARN]{NC} {msg}", file=sys.stderr)


def log_info(msg):
    print(f"{BLUE}[INFO]{NC} {msg}", file=sys.stderr)


def debug_info(msg, verbose):
    if verbose:
        log_info(msg)


def debug_warn(msg, verbose):
    if verbose:
        log_warn(msg)


# ---------------------------------------------------------------------------
# Spec-file extraction
# ---------------------------------------------------------------------------


def _specfile_macros():
    """Return extra macros for the specfile library.

    On Fedora build hosts the ``fedora`` macro is defined, which causes
    conditional blocks in OpenHPC spec files to take the wrong branch.
    Undefine it and set ``rhel 10`` instead.
    """
    import rpm as _rpm

    if _rpm.expandMacro("0%{?fedora}") != "0":
        return [("fedora", None), ("rhel", "10")]
    return []


def extract_package_info(spec_file, verbose):
    """Parse a spec file and return (name, version, source_url) or None.

    Returns ``None`` for files that should be skipped (no HTTP source).
    Raises ``RuntimeError`` on unexpected parse failures.
    """
    from specfile import Specfile
    from specfile.exceptions import SpecfileException

    try:
        spec = Specfile(
            spec_file,
            sourcedir="components",
            macros=_specfile_macros(),
        )
    except SpecfileException as exc:
        debug_warn(f"Failed to parse {spec_file}: {exc}", verbose)
        raise RuntimeError(f"Failed to parse {spec_file}") from exc

    name = spec.expanded_name
    version = spec.expanded_version

    if not name or not version:
        debug_warn(
            f"Incomplete package info for {spec_file}: "
            f"name='{name}' version='{version}'",
            verbose,
        )
        raise RuntimeError(f"Incomplete package info for {spec_file}")

    # Extract source URL - only consider URLs starting with http
    source0 = None
    with spec.sources() as sources:
        for src in sources:
            loc = src.expanded_location
            if loc and loc.startswith("http"):
                source0 = loc
                break

    if not source0:
        debug_info(f"Skipping {name}: no HTTP upstream source defined", verbose)
        return None

    return (name, version, source0)


# ---------------------------------------------------------------------------
# URL parsers
# ---------------------------------------------------------------------------


def parse_github_repo(url):
    """Return 'owner/repo' if *url* is a GitHub source URL, else None."""
    m = re.search(r"https://github\.com/([^/]+)/([^/]+)/", url)
    if m:
        return f"{m.group(1)}/{m.group(2)}"
    return None


def parse_gitlab_repo(url):
    """Return (hostname, 'owner/repo') if *url* is a GitLab source URL."""
    m = re.search(r"https://([^/]+)/([^/]+)/([^/]+)/-", url)
    if m:
        return (m.group(1), f"{m.group(2)}/{m.group(3)}")
    return None


def parse_pypi_package(url):
    """Return the package name if *url* is a PyPI source URL."""
    m = re.search(
        r"https://(files\.pythonhosted\.org|pypi\.io)"
        r"/packages/source/[^/]+/([^/]+)/",
        url,
    )
    if m:
        return m.group(2)
    return None


def parse_jsc_perftools_package(url):
    """Return the project name if *url* is a JSC perftools URL."""
    m = re.search(
        r"https?://perftools\.pages\.jsc\.fz-juelich\.de/cicd/([^/]+)/",
        url,
    )
    if m:
        return m.group(1)
    return None


def parse_openmpi_source(url):
    """Return 'open-mpi/ompi' if *url* is an Open MPI download URL."""
    if re.search(r"https?://www\.open-mpi\.org/software/ompi/", url):
        return "open-mpi/ompi"
    return None


def parse_pnetcdf_source(url):
    """Return True if *url* is a PnetCDF download from parallel-netcdf.github.io."""
    if re.search(r"https?://parallel-netcdf\.github\.io/Release/", url):
        return True
    return None


def parse_uoregon_tau_package(url):
    """Return (directory, project) for a UOregon TAU/PDT source URL.

    Examples:
      https://www.cs.uoregon.edu/research/tau/tau_releases/tau-2.34.1.tar.gz
        -> ("tau_releases", "tau")
      https://www.cs.uoregon.edu/research/tau/pdt_releases/pdtoolkit-3.25.1.tar.gz
        -> ("pdt_releases", "pdtoolkit")
      https://www.cs.uoregon.edu/research/paracomp/pdtoolkit/Download/pdtoolkit-3.25.1.tar.gz
        -> ("pdt_releases", "pdtoolkit")
    """
    # Direct tau_releases or pdt_releases URL
    m = re.search(
        r"https?://www\.cs\.uoregon\.edu/research/tau/"
        r"(tau_releases|pdt_releases)/([^/]+)-[\d]",
        url,
    )
    if m:
        return (m.group(1), m.group(2))
    # PDT via paracomp/pdtoolkit/Download path (maps to pdt_releases)
    m = re.search(
        r"https?://www\.cs\.uoregon\.edu/research/paracomp/"
        r"pdtoolkit/Download/([^/]+)-[\d]",
        url,
    )
    if m:
        return ("pdt_releases", m.group(1))
    return None


def parse_bsc_ftp_package(url):
    """Return (directory, project) for a BSC tools FTP URL.

    Examples:
      https://ftp.tools.bsc.es/dimemas/dimemas-5.4.2-src.tar.bz2
        -> ("dimemas", "dimemas")
      https://ftp.tools.bsc.es/wxparaver/wxparaver-4.10.4-src.tar.bz2
        -> ("wxparaver", "wxparaver")
    """
    m = re.search(r"https?://ftp\.tools\.bsc\.es/([^/]+)/([^/]+)-[^/]+-src\.", url)
    if m:
        return (m.group(1), m.group(2))
    return None


# ---------------------------------------------------------------------------
# Version fetchers
# ---------------------------------------------------------------------------


def _github_headers(token):
    headers = {}
    if token:
        headers["Authorization"] = f"token {token}"
    return headers


def get_latest_github_release(repo, token, check_prereleases, verbose):
    """Return the latest release tag name for *repo*.

    Raises ``RateLimitError`` on 403 with a rate-limit message.
    Returns ``None`` if no releases are found.
    """
    headers = _github_headers(token)
    if not token:
        time.sleep(1)

    # Try /releases/latest first (unless checking prereleases)
    if not check_prereleases:
        url = f"https://api.github.com/repos/{repo}/releases/latest"
        r = requests.get(url, headers=headers, timeout=30)
        if r.status_code == 403 and "rate limit" in r.text.lower():
            raise RateLimitError("GitHub API rate limit exceeded")
        if r.ok:
            data = r.json()
            tag = data.get("tag_name")
            if tag:
                return tag
        else:
            debug_info(f"Latest endpoint failed for {repo}: {r.status_code}", verbose)

    # Fall back to releases list
    url = f"https://api.github.com/repos/{repo}/releases"
    r = requests.get(url, headers=headers, timeout=30)
    if r.status_code == 403 and "rate limit" in r.text.lower():
        raise RateLimitError("GitHub API rate limit exceeded")
    if not r.ok:
        debug_warn(f"GitHub API error for {repo}: {r.status_code}", verbose)
        return None

    releases = r.json()
    if not isinstance(releases, list) or len(releases) == 0:
        debug_info(f"No releases found for {repo}", verbose)
        return None

    tag = releases[0].get("tag_name")
    if not tag:
        debug_warn(f"No suitable releases found for {repo}", verbose)
        return None
    return tag


def get_latest_github_tag(repo, token, exclude_patterns, verbose):
    """Return the latest tag for *repo*, filtering out *exclude_patterns*.

    Raises ``RateLimitError`` on 403 with a rate-limit message.
    """
    headers = _github_headers(token)
    if not token:
        time.sleep(1)

    url = f"https://api.github.com/repos/{repo}/tags?per_page=100"
    r = requests.get(url, headers=headers, timeout=30)
    if r.status_code == 403 and "rate limit" in r.text.lower():
        raise RateLimitError("GitHub API rate limit exceeded")
    if not r.ok:
        debug_warn(f"GitHub API error for {repo}: {r.status_code}", verbose)
        return None

    tags = r.json()
    if not isinstance(tags, list) or len(tags) == 0:
        debug_info(f"No tags found for {repo}", verbose)
        return None

    for tag_obj in tags:
        tag_name = tag_obj.get("name", "")
        skip = False
        for pat in exclude_patterns:
            if pat in tag_name:
                debug_info(
                    f"Skipping tag {tag_name}: matches exclude pattern '{pat}'",
                    verbose,
                )
                skip = True
                break
        if not skip:
            return tag_name

    debug_warn(f"No suitable tags found for {repo} after filtering", verbose)
    return None


def get_latest_gitlab_release(hostname, project_path, verbose):
    """Return the latest tag for a GitLab project (public API)."""
    # Find project ID first
    search_name = project_path.split("/")[-1]
    search_url = f"https://{hostname}/api/v4/projects?search={search_name}"
    r = requests.get(search_url, timeout=30)
    if not r.ok:
        debug_warn(
            f"Failed to search for GitLab project {project_path} on {hostname}",
            verbose,
        )
        return None

    projects = r.json()
    if isinstance(projects, dict) and "message" in projects:
        debug_warn(
            f"GitLab API error for {project_path}: {projects['message']}",
            verbose,
        )
        return None

    project_id = None
    for p in projects:
        if p.get("path_with_namespace") == project_path:
            project_id = p.get("id")
            break

    if not project_id:
        debug_warn(
            f"No GitLab project found for {project_path} on {hostname}",
            verbose,
        )
        return None

    debug_info(f"Found GitLab project ID: {project_id}", verbose)

    # Fetch tags
    tags_url = f"https://{hostname}/api/v4/projects/{project_id}/repository/tags"
    r = requests.get(tags_url, timeout=30)
    if not r.ok:
        debug_warn(
            f"Failed to fetch tags for GitLab project {project_id} on {hostname}",
            verbose,
        )
        return None

    tags = r.json()
    if isinstance(tags, dict) and "message" in tags:
        debug_warn(
            f"GitLab API error for project {project_id}: {tags['message']}",
            verbose,
        )
        return None

    if not isinstance(tags, list) or len(tags) == 0:
        debug_info(
            f"No tags found for GitLab project {project_id} on {hostname}",
            verbose,
        )
        return None

    tag_name = tags[0].get("name")
    if not tag_name:
        debug_warn(f"No suitable tags found for GitLab project {project_id}", verbose)
        return None

    return tag_name


def get_latest_pypi_version(package_name, verbose):
    """Return the latest version from PyPI for *package_name*."""
    url = f"https://pypi.org/pypi/{package_name}/json"
    debug_info(f"Fetching PyPI package info from {url}", verbose)

    r = requests.get(url, timeout=30)
    if not r.ok:
        debug_warn(f"Failed to fetch PyPI package info for {package_name}", verbose)
        return None

    data = r.json()
    version = data.get("info", {}).get("version")
    if not version:
        debug_warn(f"No version found for PyPI package {package_name}", verbose)
        return None
    return version


def get_latest_jsc_perftools_version(project_name, check_prereleases, verbose):
    """Return the latest version from JSC perftools overview page."""
    base_url = f"https://perftools.pages.jsc.fz-juelich.de/cicd/{project_name}/"
    debug_info(f"Fetching JSC perftools package info from {base_url}", verbose)

    r = requests.get(base_url, timeout=30)
    if not r.ok or "<html" not in r.text.lower():
        debug_warn(f"Failed to fetch JSC perftools page for {project_name}", verbose)
        return None

    pattern = re.compile(
        rf"tags/{re.escape(project_name)}-([^/]+)/"
        rf"{re.escape(project_name)}-[^\"]+\.tar\.gz"
    )
    matches = pattern.findall(r.text)[:20]

    if not matches:
        debug_warn(
            f"No version tags found for JSC perftools package {project_name}",
            verbose,
        )
        return None

    for version in matches:
        if not check_prereleases:
            if re.search(r"-(rc|alpha|beta|pre|dev)\d*$", version):
                debug_info(f"Skipping pre-release version: {version}", verbose)
                continue
        return version

    debug_warn(
        f"No suitable version found for JSC perftools package {project_name}",
        verbose,
    )
    return None


def get_latest_gnu_version(project, verbose):
    """Return the latest version from the GNU FTP mirror."""
    base_url = f"https://ftpmirror.gnu.org/gnu/{project}/"
    debug_info(f"Checking GNU {project} directory listing at {base_url}", verbose)

    r = requests.get(base_url, timeout=30)
    if not r.ok:
        debug_warn(f"Failed to fetch directory listing for GNU {project}", verbose)
        return None

    listing = r.text

    if project == "gcc":
        versions = re.findall(r'href="gcc-(\d+\.\d+\.\d+)/"', listing)
    elif project in ("gmp", "mpc", "mpfr"):
        versions = re.findall(
            rf'href="{re.escape(project)}-(\d+\.\d+(?:\.\d+)?)'
            r'\.(?:tar\.(?:gz|bz2|xz)|tgz)"',
            listing,
        )
    else:
        debug_warn(f"Unknown GNU project: {project}", verbose)
        return None

    if not versions:
        debug_warn(f"No version found for GNU {project}", verbose)
        return None

    # Sort using RPM version comparison and return the highest
    versions.sort(key=_version_sort_key)
    return versions[-1]


def _version_sort_key(v):
    """Simple numeric-aware sort key for version strings."""
    parts = re.split(r"[.\-]", v)
    result = []
    for p in parts:
        try:
            result.append((0, int(p)))
        except ValueError:
            result.append((1, p))
    return result


def get_latest_bsc_ftp_version(directory, project, check_prereleases, verbose):
    """Return the latest version from the BSC tools FTP directory listing.

    Parses source tarballs named ``<project>-<version>-src.tar.*`` from the
    directory listing at ``https://ftp.tools.bsc.es/<directory>/``.
    """
    base_url = f"https://ftp.tools.bsc.es/{directory}/"
    debug_info(f"Checking BSC FTP directory listing at {base_url}", verbose)

    r = requests.get(base_url, timeout=30)
    if not r.ok:
        debug_warn(
            f"Failed to fetch BSC FTP directory listing for {directory}", verbose
        )
        return None

    # Extract versions from source tarball links:
    #   <project>-<version>-src.tar.{bz2,gz,xz}
    # Skip "latest" and other non-version entries
    pattern = re.compile(
        rf'href="{re.escape(project)}-([^"]+)-src\.tar\.\w+"', re.IGNORECASE
    )
    raw_versions = pattern.findall(r.text)

    if not raw_versions:
        debug_warn(f"No source tarballs found for {project} on BSC FTP", verbose)
        return None

    # Filter out non-version entries (e.g. "latest", "deepsea-v2")
    versions = []
    for v in raw_versions:
        if v == "latest" or not re.match(r"\d", v):
            continue
        if not check_prereleases:
            if re.search(r"(rc|alpha|beta|pre|dev)\d*", v, re.IGNORECASE):
                debug_info(f"Skipping pre-release version: {v}", verbose)
                continue
        versions.append(v)

    if not versions:
        debug_warn(f"No suitable version found for {project} on BSC FTP", verbose)
        return None

    # Sort and return the highest version
    versions.sort(key=_version_sort_key)
    return versions[-1]


def get_latest_uoregon_tau_version(directory, project, check_prereleases, verbose):
    """Return the latest version from a UOregon TAU/PDT directory listing.

    Parses tarballs named ``<project>-<version>.tar.gz`` from the
    directory listing at
    ``https://www.cs.uoregon.edu/research/tau/<directory>/``.
    """
    base_url = f"https://www.cs.uoregon.edu/research/tau/{directory}/"
    debug_info(f"Checking UOregon TAU directory listing at {base_url}", verbose)

    r = requests.get(base_url, timeout=30)
    if not r.ok:
        debug_warn(
            f"Failed to fetch UOregon TAU directory listing for {directory}",
            verbose,
        )
        return None

    pattern = re.compile(
        rf'href="{re.escape(project)}-(\d+[^"]*?)\.tar\.gz"', re.IGNORECASE
    )
    raw_versions = pattern.findall(r.text)

    if not raw_versions:
        debug_warn(f"No source tarballs found for {project} in {directory}", verbose)
        return None

    versions = []
    for v in raw_versions:
        # Only accept purely numeric versions (digits, dots, dashes)
        # to skip entries like 2.8b10.2, 2.19akm, 3.6a, 3.19p1
        if not re.match(r"\d+(\.\d+)*$", v):
            debug_info(f"Skipping non-numeric version: {v}", verbose)
            continue
        versions.append(v)

    if not versions:
        debug_warn(f"No suitable version found for {project} in {directory}", verbose)
        return None

    versions.sort(key=_version_sort_key)
    return versions[-1]


def get_latest_pnetcdf_version(check_prereleases, verbose):
    """Return the latest PnetCDF version from the download page."""
    url = "https://parallel-netcdf.github.io/wiki/Download.html"
    debug_info(f"Fetching PnetCDF download page at {url}", verbose)

    r = requests.get(url, timeout=30)
    if not r.ok:
        debug_warn("Failed to fetch PnetCDF download page", verbose)
        return None

    # Extract versions from tarball links:
    #   pnetcdf-<version>.tar.gz  or  parallel-netcdf-<version>.tar.gz
    pattern = re.compile(r"(?:pnetcdf|parallel-netcdf)-(\d+\.\d+[^\"]*?)\.tar\.gz")
    raw_versions = pattern.findall(r.text)

    if not raw_versions:
        debug_warn("No versions found on PnetCDF download page", verbose)
        return None

    # Deduplicate while preserving order
    seen = set()
    versions = []
    for v in raw_versions:
        if v not in seen:
            seen.add(v)
            if not check_prereleases:
                if re.search(r"(alpha|beta|rc|pre|dev)", v, re.IGNORECASE):
                    debug_info(f"Skipping pre-release version: {v}", verbose)
                    continue
            versions.append(v)

    if not versions:
        debug_warn("No suitable PnetCDF version found", verbose)
        return None

    versions.sort(key=_version_sort_key)
    return versions[-1]


def get_latest_release_monitoring_version(project_ref, check_prereleases, verbose):
    """Return the latest version from release-monitoring.org."""
    if project_ref.startswith("id:"):
        project_id = project_ref[3:]
        url = f"https://release-monitoring.org/api/project/{project_id}"
        debug_info(
            f"Checking release-monitoring.org project ID: {project_id}",
            verbose,
        )
        r = requests.get(url, timeout=30)
        if not r.ok:
            debug_warn(
                "Failed to fetch release-monitoring.org data for "
                f"project ID {project_id}",
                verbose,
            )
            return None

        data = r.json()
        if check_prereleases:
            versions = data.get("versions", [])
            return versions[0] if versions else None
        stable = data.get("stable_versions", [])
        if stable:
            return stable[0]
        return data.get("version")
    else:
        url = f"https://release-monitoring.org/api/v2/projects/?name={project_ref}"
        debug_info(f"Checking release-monitoring.org for: {project_ref}", verbose)
        r = requests.get(url, timeout=30)
        if not r.ok:
            debug_warn(
                f"Failed to fetch release-monitoring.org data for {project_ref}",
                verbose,
            )
            return None

        data = r.json()
        total = data.get("total_items", 0)
        if not total:
            debug_info(
                f"No project found on release-monitoring.org for: {project_ref}",
                verbose,
            )
            return None

        items = data.get("items", [])
        if not items:
            return None

        item = items[0]
        if check_prereleases:
            versions = item.get("versions", [])
            return versions[0] if versions else None
        stable = item.get("stable_versions", [])
        if stable:
            return stable[0]
        return item.get("version")


# ---------------------------------------------------------------------------
# Version normalization & comparison
# ---------------------------------------------------------------------------


def normalize_version(version):
    """Normalize a tag/version string to a bare version number."""
    # trilinos: trilinos-release-16-1-0 -> 16.1.0
    m = re.match(r"trilinos-release-(\d+)-(\d+)-(\d+)", version)
    if m:
        return f"{m.group(1)}.{m.group(2)}.{m.group(3)}"

    # IMB: IMB-v2021.10 -> 2021.10
    m = re.match(r"IMB-v(.+)", version)
    if m:
        return m.group(1)

    # munge: munge-0.5.16 -> 0.5.16
    m = re.match(r"munge-(.+)", version)
    if m:
        return m.group(1)

    # genders: genders-1-32-1 -> 1.32  (drop last component)
    m = re.match(r"genders-(\d+)-(\d+)-(\d+)", version)
    if m:
        return f"{m.group(1)}.{m.group(2)}"

    # conman: conman-0.3.1 -> 0.3.1
    m = re.match(r"conman-(.+)", version)
    if m:
        return m.group(1)

    # pdsh: pdsh-2.35 -> 2.35
    m = re.match(r"pdsh-(.+)", version)
    if m:
        return m.group(1)

    # llvm: llvmorg-21.1.3 -> 21.1.3
    m = re.match(r"llvmorg-(.+)", version)
    if m:
        return m.group(1)

    # hdf5/phdf5: hdf5_1.14.6 -> 1.14.6
    m = re.match(r"hdf5_(.+)", version)
    if m:
        return m.group(1)

    # papi: papi-7-2-0-t -> 7.2.0
    m = re.match(r"^papi-(\d+-\d+.*)-t$", version)
    if m:
        return m.group(1).replace("-", ".")

    # Generic: strip v, release-, rel- prefixes
    for prefix in ("v", "release-", "rel-"):
        if version.startswith(prefix):
            version = version[len(prefix) :]
            break

    return version


def compare_versions(current, latest):
    """Compare *current* and *latest* using rpm.labelCompare.

    Returns one of ``'older'``, ``'same'``, ``'newer'``.
    """
    import rpm

    current = normalize_version(current)
    latest = normalize_version(latest)

    rc = rpm.labelCompare(("0", current, "1"), ("0", latest, "1"))
    if rc < 0:
        return "older"
    if rc == 0:
        return "same"
    return "newer"


# ---------------------------------------------------------------------------
# OHPC name handling
# ---------------------------------------------------------------------------


def strip_ohpc_suffixes(name):
    """Strip OpenHPC package-naming suffixes to get the upstream name."""
    m = re.match(r"^(.*)-gnu\d+-openmpi\d+-ohpc$", name)
    if m:
        return m.group(1)
    m = re.match(r"^(.*)-gnu\d+-ohpc$", name)
    if m:
        return m.group(1)
    m = re.match(r"^(.*)-ohpc$", name)
    if m:
        return m.group(1)
    return name


# ---------------------------------------------------------------------------
# Checker dispatch helpers
# ---------------------------------------------------------------------------


def _status_from_comparison(comparison):
    return {
        "older": UPDATE_AVAILABLE,
        "same": UP_TO_DATE,
        "newer": AHEAD,
    }.get(comparison, UNKNOWN)


def try_github(source_url, name, current_version, config):
    """Check GitHub for a newer release.  Returns list[Result] or None."""
    repo = parse_github_repo(source_url)
    if repo is None:
        return None

    debug_info(f"Checking GitHub repo: {repo}", config.verbose)

    try:
        latest_tag = get_latest_github_release(
            repo, config.token, config.prereleases, config.verbose
        )
    except RateLimitError:
        log_warn(
            "GitHub API rate limit exceeded. "
            "Use GITHUB_TOKEN environment variable for higher limits."
        )
        return [Result(name, current_version, RATE_LIMITED, RATE_LIMITED, repo)]

    if latest_tag is None:
        return [Result(name, current_version, ERROR, ERROR, repo)]

    cmp = compare_versions(current_version, latest_tag)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_tag} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_tag, status, repo)]


def try_gitlab(source_url, name, current_version, config):
    """Check GitLab for a newer release.  Returns list[Result] or None."""
    parsed = parse_gitlab_repo(source_url)
    if parsed is None:
        return None

    hostname, project_path = parsed
    debug_info(
        f"Checking GitLab repo: {project_path} on {hostname}",
        config.verbose,
    )

    latest_tag = get_latest_gitlab_release(hostname, project_path, config.verbose)
    repo_id = f"{hostname}/{project_path}"

    if latest_tag is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_tag)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_tag} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_tag, status, repo_id)]


def try_pypi(source_url, name, current_version, config):
    """Check PyPI for a newer version.  Returns list[Result] or None."""
    pkg = parse_pypi_package(source_url)
    if pkg is None:
        return None

    debug_info(f"Checking PyPI package: {pkg}", config.verbose)
    latest_version = get_latest_pypi_version(pkg, config.verbose)
    repo_id = f"pypi.org/project/{pkg}"

    if latest_version is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_version, status, repo_id)]


def try_jsc_perftools(source_url, name, current_version, config):
    """Check JSC perftools for a newer version.  Returns list[Result] or None."""
    project = parse_jsc_perftools_package(source_url)
    if project is None:
        return None

    debug_info(f"Checking JSC perftools package: {project}", config.verbose)
    latest_version = get_latest_jsc_perftools_version(
        project, config.prereleases, config.verbose
    )
    repo_id = f"perftools.pages.jsc.fz-juelich.de/cicd/{project}"

    if latest_version is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_version, status, repo_id)]


def try_openmpi(source_url, name, current_version, config):
    """Check Open MPI GitHub tags.  Returns list[Result] or None."""
    repo = parse_openmpi_source(source_url)
    if repo is None:
        return None

    debug_info(f"Checking Open MPI GitHub tags: {repo}", config.verbose)

    try:
        latest_tag = get_latest_github_tag(
            repo, config.token, ["rc", "amzn"], config.verbose
        )
    except RateLimitError:
        log_warn(
            "GitHub API rate limit exceeded. "
            "Use GITHUB_TOKEN environment variable for higher limits."
        )
        return [Result(name, current_version, RATE_LIMITED, RATE_LIMITED, repo)]

    if latest_tag is None:
        return [Result(name, current_version, ERROR, ERROR, repo)]

    cmp = compare_versions(current_version, latest_tag)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_tag} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_tag, status, repo)]


def try_uoregon_tau(source_url, name, current_version, config):
    """Check UOregon TAU/PDT directory for a newer version.

    Returns list[Result] or None.
    """
    parsed = parse_uoregon_tau_package(source_url)
    if parsed is None:
        return None

    directory, project = parsed
    debug_info(f"Checking UOregon TAU package: {project}", config.verbose)
    latest_version = get_latest_uoregon_tau_version(
        directory, project, config.prereleases, config.verbose
    )
    repo_id = f"cs.uoregon.edu/research/tau/{directory}"

    if latest_version is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_version, status, repo_id)]


def try_bsc_ftp(source_url, name, current_version, config):
    """Check BSC tools FTP for a newer version.  Returns list[Result] or None."""
    parsed = parse_bsc_ftp_package(source_url)
    if parsed is None:
        return None

    directory, project = parsed
    debug_info(f"Checking BSC FTP package: {project}", config.verbose)
    latest_version = get_latest_bsc_ftp_version(
        directory, project, config.prereleases, config.verbose
    )
    repo_id = f"ftp.tools.bsc.es/{directory}"

    if latest_version is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_version, status, repo_id)]


def try_pnetcdf(source_url, name, current_version, config):
    """Check PnetCDF download page for a newer version."""
    if parse_pnetcdf_source(source_url) is None:
        return None

    debug_info("Checking PnetCDF download page", config.verbose)
    latest_version = get_latest_pnetcdf_version(config.prereleases, config.verbose)
    repo_id = "parallel-netcdf.github.io"

    if latest_version is None:
        return [Result(name, current_version, ERROR, ERROR, repo_id)]

    cmp = compare_versions(current_version, latest_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {latest_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, latest_version, status, repo_id)]


def try_release_monitoring(source_url, name, current_version, config):
    """Fallback: check release-monitoring.org.  Always returns list[Result]."""
    base_name = strip_ohpc_suffixes(name)
    rm_project = RELEASE_MONITORING_MAP.get(base_name, base_name)

    debug_info(
        f"Trying release-monitoring.org fallback for {name} (project: {rm_project})",
        config.verbose,
    )

    rm_version = get_latest_release_monitoring_version(
        rm_project, config.prereleases, config.verbose
    )

    if rm_version is None:
        return [Result(name, current_version, "-", NOT_FOUND, "N/A")]

    if rm_project.startswith("id:"):
        repo_id = f"release-monitoring.org/{rm_project[3:]}"
    else:
        repo_id = f"release-monitoring.org/{rm_project}"

    cmp = compare_versions(current_version, rm_version)
    status = _status_from_comparison(cmp)
    debug_info(
        f"Result: {name} {current_version} -> {rm_version} ({status})",
        config.verbose,
    )
    return [Result(name, current_version, rm_version, status, repo_id)]


# ---------------------------------------------------------------------------
# GNU compilers special handler
# ---------------------------------------------------------------------------


def check_gnu_compilers(spec_file, config):
    """Check for updates to the GNU compiler suite components."""
    with open(spec_file) as f:
        spec_content = f.read()

    # Extract default compiler family from OHPC_macros
    with open("components/OHPC_macros") as f:
        macros_content = f.read()

    m = re.search(r"compiler_family (gnu\d+)", macros_content)
    if m:
        default_family = m.group(1)
    else:
        debug_warn(
            "Could not determine default compiler family from OHPC_macros, "
            "defaulting to gnu15",
            config.verbose,
        )
        default_family = "gnu15"

    gnu_ver = default_family.replace("gnu", "")
    debug_info(
        f"Using default compiler family: {default_family} (version {gnu_ver})",
        config.verbose,
    )

    component_name = f"{default_family}-compilers"
    results = []

    components = [
        ("gcc", f"gnu{gnu_ver}_version", "gcc"),
        ("gmp", f"gnu{gnu_ver}_gmp_version", "gmp"),
        ("mpc", f"gnu{gnu_ver}_mpc_version", "mpc"),
        ("mpfr", f"gnu{gnu_ver}_mpfr_version", "mpfr"),
    ]

    for suffix, macro, gnu_project in components:
        m = re.search(rf"^%global {macro}\s+(\S+)", spec_content, re.MULTILINE)
        if not m:
            debug_info(
                f"No GNU {gnu_ver} {suffix} version found in spec file",
                config.verbose,
            )
            continue

        current_version = m.group(1)
        latest_version = get_latest_gnu_version(gnu_project, config.verbose)

        if latest_version is None:
            results.append(
                Result(
                    f"{component_name}-{suffix}",
                    current_version,
                    ERROR,
                    ERROR,
                    f"gnu.org/{gnu_project}",
                    spec_file=spec_file,
                )
            )
            continue

        cmp = compare_versions(current_version, latest_version)
        status = _status_from_comparison(cmp)
        results.append(
            Result(
                f"{component_name}-{suffix}",
                current_version,
                latest_version,
                status,
                f"gnu.org/{gnu_project}",
                spec_file=spec_file,
            )
        )

    return results


# ---------------------------------------------------------------------------
# Spec file updaters
# ---------------------------------------------------------------------------


def update_spec_version(spec_file, new_version, verbose):
    """Update the Version tag in a spec file using the specfile library."""
    from specfile import Specfile

    spec = Specfile(spec_file, sourcedir="components", macros=_specfile_macros())
    spec.update_version(new_version)
    spec.save()
    log_info(f"Updated {spec_file}: Version -> {new_version}")


def update_gnu_spec_version(spec_file, result_name, new_version, verbose):
    """Update a %global macro version in the GNU compilers spec file.

    *result_name* has the form ``gnu<N>-compilers-<component>`` where
    component is one of gcc, gmp, mpc, mpfr.
    """
    # Extract the component suffix (gcc, gmp, mpc, mpfr) and version
    # number from the result name (e.g. "gnu15-compilers-gcc")
    m = re.match(r"gnu(\d+)-compilers-(\w+)", result_name)
    if not m:
        log_warn(f"Cannot parse GNU component from result name: {result_name}")
        return

    gnu_ver = m.group(1)
    component = m.group(2)
    macro_name = (
        f"gnu{gnu_ver}_{component}_version"
        if component != "gcc"
        else f"gnu{gnu_ver}_version"
    )

    with open(spec_file) as f:
        content = f.read()

    pattern = rf"(^%global {re.escape(macro_name)}\s+)\S+"
    new_content, count = re.subn(
        pattern, rf"\g<1>{new_version}", content, count=1, flags=re.MULTILINE
    )

    if count == 0:
        log_warn(f"Could not find %global {macro_name} in {spec_file}")
        return

    with open(spec_file, "w") as f:
        f.write(new_content)

    log_info(f"Updated {spec_file}: %global {macro_name} -> {new_version}")


def commit_updates(updated_results):
    """Stage updated spec files and create a signed git commit.

    The commit message includes the command line used to invoke the
    script and the upstream backend for each updated package.  All
    lines are kept within 72 characters.
    """
    if not updated_results:
        return

    # Collect unique spec files (preserving order)
    spec_files = list(dict.fromkeys(r.spec_file for r in updated_results))

    # Stage the changed spec files
    try:
        subprocess.run(
            ["git", "add"] + spec_files,
            check=True,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        log_warn("git not found; skipping commit")
        return
    except subprocess.CalledProcessError as exc:
        log_warn(f"Failed to stage spec files: {exc.stderr.strip()}")
        return

    # Check if there are actually staged changes
    rc = subprocess.run(
        ["git", "diff", "--cached", "--quiet"],
        capture_output=True,
    )
    if rc.returncode == 0:
        log_info("No changes to commit")
        return

    # Extract component group (directory under components/)
    # e.g. "components/serial-libs/gsl/SPECS/gsl.spec" -> "serial-libs"
    groups = list(
        dict.fromkeys(
            r.spec_file.split("/")[1]
            for r in updated_results
            if r.spec_file and len(r.spec_file.split("/")) > 1
        )
    )
    prefix = f"{groups[0]}: " if len(groups) == 1 else ""

    # Determine the subject line
    if len(spec_files) == 1:
        spec_name = os.path.splitext(os.path.basename(spec_files[0]))[0]
        if len(updated_results) == 1:
            r = updated_results[0]
            latest = normalize_version(r.latest_version)
            subject = f"{prefix}update {spec_name} to {latest}"
        else:
            subject = f"{prefix}update {spec_name}"
    else:
        subject = f"{prefix}update packages to latest versions"

    if len(subject) > 72:
        subject = subject[:69] + "..."

    # Build commit body -- each result gets two lines so that
    # individual lines stay within 72 characters.
    body_lines = []
    for r in updated_results:
        latest = normalize_version(r.latest_version)
        name = strip_ohpc_suffixes(r.name)
        body_lines.append(f"- {name}: {r.current_version} -> {latest}")
        body_lines.append(f"  upstream: {r.repo}")

    body_lines.append("")
    cmd_line = f"Command: `{' '.join(sys.argv)}`"
    body_lines.append(
        textwrap.fill(
            cmd_line,
            width=72,
            subsequent_indent="  ",
            break_on_hyphens=False,
        )
    )

    # Build the Signed-off-by line ourselves so we can place a
    # blank line between the Command trailer and the sign-off.
    # Using "git commit -s" would merge them into one trailer block.
    try:
        sob_name = subprocess.run(
            ["git", "config", "user.name"],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
        sob_email = subprocess.run(
            ["git", "config", "user.email"],
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
        body_lines.append("")
        body_lines.append(f"Signed-off-by: {sob_name} <{sob_email}>")
    except subprocess.CalledProcessError:
        log_warn("Could not determine git identity for sign-off")

    message = subject + "\n\n" + "\n".join(body_lines)

    try:
        subprocess.run(
            ["git", "commit", "-m", message],
            check=True,
            capture_output=True,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        log_warn(f"Failed to create commit: {exc.stderr.strip()}")
        return

    log_info(f"Created signed commit: {subject}")


# ---------------------------------------------------------------------------
# Main processing
# ---------------------------------------------------------------------------

# Ordered list of checkers.  The first one to return non-None wins.
CHECKERS = [
    try_github,
    try_gitlab,
    try_pypi,
    try_jsc_perftools,
    try_openmpi,
    try_uoregon_tau,
    try_bsc_ftp,
    try_pnetcdf,
    try_release_monitoring,  # always returns a result (fallback)
]


def process_spec_file(spec_file, config):
    """Process a single spec file.  Returns a list of Result objects.

    Returns an empty list when the file should be silently skipped.
    Raises ``RateLimitError`` if processing should stop due to rate limits.
    """
    debug_info(f"Processing {os.path.basename(spec_file)}", config.verbose)

    # Special handling for GNU compilers
    if spec_file.endswith("gnu-compilers/SPECS/gnu-compilers.spec"):
        debug_info(
            "Detected GNU compilers spec file - using special handling",
            config.verbose,
        )
        return check_gnu_compilers(spec_file, config)

    try:
        info = extract_package_info(spec_file, config.verbose)
    except RuntimeError:
        return []

    if info is None:
        return []

    name, current_version, source_url = info

    for checker in CHECKERS:
        results = checker(source_url, name, current_version, config)
        if results is not None:
            # Check if we hit a rate limit
            for r in results:
                if r.status == RATE_LIMITED:
                    raise RateLimitError("Rate limit reached")
            # Attach spec_file to each result
            for r in results:
                r.spec_file = spec_file
            return results

    # Should not reach here because try_release_monitoring always returns
    return [Result(name, current_version, "-", NOT_FOUND, "N/A", spec_file=spec_file)]


# ---------------------------------------------------------------------------
# Output formatting
# ---------------------------------------------------------------------------


def display_results_table(results):
    """Print results in a colored table to stdout."""
    fmt = "%-35s %-15s %-25s %-20s %s"
    print(fmt % ("PACKAGE", "CURRENT", "LATEST", "STATUS", "REPOSITORY"))
    print(fmt % ("-" * 35, "-" * 15, "-" * 25, "-" * 20, "-" * 50))

    color_map = {
        UPDATE_AVAILABLE: YELLOW,
        UP_TO_DATE: GREEN,
        AHEAD: BLUE,
        RATE_LIMITED: YELLOW,
        NOT_FOUND: YELLOW,
        ERROR: RED,
    }

    for r in results:
        color = color_map.get(r.status, "")
        reset = NC if color else ""
        print(
            f"{color}"
            + fmt % (r.name, r.current_version, r.latest_version, r.status, r.repo)
            + f"{reset}"
        )


def display_results_json(results):
    """Print results as a JSON array to stdout."""
    data = []
    for r in results:
        entry = {
            "name": r.name,
            "current_version": r.current_version,
            "latest_version": r.latest_version,
            "status": r.status,
            "repository": r.repo,
        }
        if r.spec_file is not None:
            entry["spec_file"] = r.spec_file
        data.append(entry)
    print(json.dumps(data, indent=2))


_REPO_URL_MAP = {
    "gnu.org/": "https://ftpmirror.gnu.org/gnu/",
    "cs.uoregon.edu/research/tau/": "https://www.cs.uoregon.edu/research/tau/",
    "ftp.tools.bsc.es/": "https://ftp.tools.bsc.es/",
    "pypi.org/project/": "https://pypi.org/project/",
    "perftools.pages.jsc.fz-juelich.de/cicd/": (
        "https://perftools.pages.jsc.fz-juelich.de/cicd/"
    ),
}


def _repo_url(repo):
    """Generate a clickable URL for the repository identifier.

    All base URLs are hardcoded; only the path component extracted from
    *repo* is appended, so the host can never be influenced by the repo
    identifier string.
    """
    if repo == "parallel-netcdf.github.io":
        return "https://parallel-netcdf.github.io/wiki/Download.html"

    for prefix, base_url in _REPO_URL_MAP.items():
        if repo.startswith(prefix):
            path = repo[len(prefix) :]
            return f"{base_url}{path}/"

    if repo.startswith("release-monitoring.org/"):
        ref = repo[len("release-monitoring.org/") :]
        if re.match(r"^\d+$", ref):
            return f"https://release-monitoring.org/project/{ref}/"
        return "https://release-monitoring.org/projects/?pattern=" + ref

    if "/" in repo and repo.count("/") == 1:
        # GitHub: owner/repo
        owner, name = repo.split("/", 1)
        return f"https://github.com/{owner}/{name}"

    # GitLab: hostname/owner/repo
    parts = repo.split("/", 2)
    if len(parts) == 2:
        return f"https://{parts[0]}/{parts[1]}"
    return f"https://{parts[0]}/{parts[1]}/{parts[2]}"


def display_results_markdown(results, no_glow):
    """Print results as a Markdown table to stdout."""
    status_emoji = {
        UPDATE_AVAILABLE: "\U0001f504",  # arrows
        UP_TO_DATE: "\u2705",  # check
        AHEAD: "\U0001f680",  # rocket
        RATE_LIMITED: "\u23f1\ufe0f",  # stopwatch
        NOT_FOUND: "\U0001f536",  # diamond
        ERROR: "\u274c",  # cross
    }

    lines = [
        "# OpenHPC Package Release Status",
        "",
        "| Package | Current | Latest | Status | Repository |",
        "|---------|---------|--------|--------|------------|",
    ]

    for r in results:
        emoji = status_emoji.get(r.status, "\u2753")
        url = _repo_url(r.repo)
        lines.append(
            f"| {r.name:<30s} "
            f"| {r.current_version:<15s} "
            f"| {r.latest_version:<20s} "
            f"| {emoji} {r.status:<25s} "
            f"| [{r.repo}]({url}) |"
        )

    md_text = "\n".join(lines) + "\n"

    if not no_glow and shutil.which("glow"):
        proc = subprocess.run(
            ["glow", "-w", "0", "-"],
            input=md_text,
            text=True,
        )
        if proc.returncode != 0:
            print(md_text, end="")
    else:
        print(md_text, end="")


# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------


def show_summary(results):
    """Print summary statistics to stderr."""
    if not results:
        return

    total = len(results)
    counts = {}
    for r in results:
        counts[r.status] = counts.get(r.status, 0) + 1

    print(file=sys.stderr)
    log_info("Summary:")
    print(f"  Total packages checked: {total}", file=sys.stderr)
    print(
        f"  Updates available: {counts.get(UPDATE_AVAILABLE, 0)}",
        file=sys.stderr,
    )
    print(f"  Up to date: {counts.get(UP_TO_DATE, 0)}", file=sys.stderr)
    print(f"  Ahead of latest: {counts.get(AHEAD, 0)}", file=sys.stderr)
    rl = counts.get(RATE_LIMITED, 0)
    if rl:
        print(f"  Rate limited: {rl}", file=sys.stderr)
    nf = counts.get(NOT_FOUND, 0)
    if nf:
        print(f"  Not found: {nf}", file=sys.stderr)
    print(f"  Errors: {counts.get(ERROR, 0)}", file=sys.stderr)


# ---------------------------------------------------------------------------
# CLI & main
# ---------------------------------------------------------------------------


class Config:
    """Configuration parsed from command-line arguments."""

    __slots__ = (
        "verbose",
        "prereleases",
        "output",
        "token",
        "no_glow",
        "package",
        "update",
    )

    def __init__(self, args):
        self.verbose = args.verbose
        self.prereleases = args.prereleases
        self.output = args.output
        self.token = args.token or os.environ.get("GITHUB_TOKEN", "")
        self.no_glow = args.no_glow
        self.package = args.package
        self.update = args.update


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        description=(
            "Check for newer releases of OpenHPC packages by analyzing "
            "spec files.  Supports GitHub, GitLab, PyPI, JSC perftools, "
            "UOregon TAU/PDT, BSC tools FTP, PnetCDF, Open MPI, GNU FTP, "
            "and release-monitoring.org (Anitya) as a fallback."
        ),
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose output"
    )
    parser.add_argument(
        "-p",
        "--prereleases",
        action="store_true",
        help="Include pre-releases in version checks",
    )
    parser.add_argument(
        "-o",
        "--output",
        choices=["table", "json", "markdown"],
        default="table",
        help="Output format (default: table)",
    )
    parser.add_argument(
        "-t",
        "--token",
        default="",
        help="GitHub API token (or set GITHUB_TOKEN env var)",
    )
    parser.add_argument(
        "--no-glow",
        action="store_true",
        help="Disable glow formatting for markdown output",
    )
    parser.add_argument(
        "--update",
        action="store_true",
        help="Automatically update spec file(s) with the newer detected version",
    )
    parser.add_argument(
        "package",
        nargs="?",
        default=None,
        help="Check only a specific package (matched against spec file path "
        "or package name)",
    )
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv)
    config = Config(args)

    if not os.path.exists("components/OHPC_macros"):
        log_error("This script must be run from the OpenHPC top-level directory")
        sys.exit(1)

    log_info("Scanning for OpenHPC packages with supported upstream sources...")

    spec_files = sorted(glob.glob("components/**/*.spec", recursive=True))

    if config.package:
        needle = config.package.lower()
        # Try exact match on base name (without extension) first
        filtered = [
            s
            for s in spec_files
            if needle == os.path.splitext(os.path.basename(s))[0].lower()
        ]
        if not filtered:
            # Fall back to substring match on the full path
            filtered = [s for s in spec_files if needle in s.lower()]
        if not filtered:
            # Try substring match against the base name without extension
            filtered = [
                s
                for s in spec_files
                if needle in os.path.splitext(os.path.basename(s))[0].lower()
            ]
        if not filtered:
            log_error(f"No spec files matched '{config.package}'")
            sys.exit(1)
        spec_files = filtered

    log_info(f"Found {len(spec_files)} spec files to process")

    all_results = []
    processed = 0
    rate_limited = False

    for spec_file in spec_files:
        try:
            results = process_spec_file(spec_file, config)
        except RateLimitError:
            # Record the rate-limited entry that triggered this
            try:
                info = extract_package_info(spec_file, config.verbose)
            except RuntimeError:
                info = None
            if info:
                name, current_version, _ = info
                all_results.append(
                    Result(
                        name,
                        current_version,
                        RATE_LIMITED,
                        RATE_LIMITED,
                        "N/A",
                    )
                )
            rate_limited = True
            log_warn("Rate limit reached. Stopping further GitHub API requests.")
            break

        all_results.extend(results)
        processed += 1

    if rate_limited:
        log_warn("Processing stopped due to GitHub API rate limiting")
        log_info("To process all packages, set GITHUB_TOKEN environment variable")
        log_info("Get a token at: https://github.com/settings/tokens")

    log_info(f"Processed {processed} spec files")

    # Sort results alphabetically by package name
    all_results.sort(key=lambda r: r.name)

    # Display results
    if all_results:
        if config.output == "table":
            display_results_table(all_results)
        elif config.output == "json":
            display_results_json(all_results)
        elif config.output == "markdown":
            display_results_markdown(all_results, config.no_glow)
    else:
        log_info("No supported upstream sources found")

    show_summary(all_results)

    # Apply updates if requested
    if config.update:
        updatable = [r for r in all_results if r.status == UPDATE_AVAILABLE]
        if not updatable:
            log_info("No updates to apply")
        else:
            updated = []
            for r in updatable:
                if r.spec_file is None:
                    log_warn(f"No spec file recorded for {r.name}, skipping")
                    continue
                latest = normalize_version(r.latest_version)
                if (
                    r.name.endswith("-gcc")
                    or r.name.endswith("-gmp")
                    or r.name.endswith("-mpc")
                    or r.name.endswith("-mpfr")
                ):
                    # GNU compiler components use %global macros
                    update_gnu_spec_version(
                        r.spec_file,
                        r.name,
                        latest,
                        config.verbose,
                    )
                else:
                    update_spec_version(
                        r.spec_file,
                        latest,
                        config.verbose,
                    )
                updated.append(r)
            commit_updates(updated)

    # Exit code: 1 if updates available, 0 otherwise
    updates = sum(1 for r in all_results if r.status == UPDATE_AVAILABLE)
    sys.exit(1 if updates > 0 else 0)


if __name__ == "__main__":
    main()
