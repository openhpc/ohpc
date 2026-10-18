#!/usr/bin/env python3
"""
Assemble a multi-version static site for the recipe explorer.

Each ref (a branch or tag of this repository) is exported with ``git archive``
into ``<out>/<ref>/`` as its ``docs/install`` tree, its ``manifest.json`` is
generated there, and one ``<out>/versions.json`` lists them so the page's
version switcher can offer them. The page itself is unchanged: a bare
``docs/install`` checkout has no ``versions.json`` above it and shows no
switcher.

The explorer that renders a ref is the one committed in that ref — its
``engine.js`` must match that ref's ``mkdoc.py``. A ref that predates the
explorer gets the current one overlaid (``--allow-overlay``), marked as such
in ``versions.json``; expect its script section markers to differ from the
released recipe.sh.

Usage (from docs/install; the git repository is found from there):
    python3 explorer/mksite.py --out ../../_site 4.x=development v4.1.GA.docs=4.1
    python3 explorer/mksite.py --out _site HEAD=development    # the checkout itself

A ref is written as ``REF`` or ``REF=LABEL``. The first ref is the default
(the site root redirects to it).
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent  # docs/install/explorer
INSTALL = HERE.parent  # docs/install


def git(*args: str, cwd: Path = INSTALL) -> str:
    proc = subprocess.run(
        ["git", *args], cwd=cwd, check=False, capture_output=True, text=True
    )
    if proc.returncode != 0:
        sys.exit(f"git {' '.join(args)} failed in {cwd}:\n{proc.stderr.strip()}")
    return proc.stdout.strip()


def resolve_ref(ref: str) -> str:
    """The ref as git knows it here: `3.x` itself, or `origin/3.x` in a CI
    clone that only has remote-tracking branches."""
    remotes = git("remote").split()
    remotes.sort(key=lambda r: (r != "origin", r))  # origin first, as in a CI clone
    prefix = git("rev-parse", "--show-prefix").rstrip("/")  # docs/install
    for candidate in (ref, *[f"{r}/{ref}" for r in remotes]):
        exists = subprocess.run(
            ["git", "cat-file", "-e", f"{candidate}:{prefix}"],
            cwd=INSTALL,
            check=False,
            capture_output=True,
        )
        if exists.returncode == 0:  # a commit that has docs/install
            return candidate
    sys.exit(
        f"error: unknown ref {ref!r} (no local branch, tag, or remote-tracking "
        f"branch of that name contains {prefix})"
    )


def safe_dir(ref: str) -> str:
    """Directory name for a ref: ``refs/heads/4.x`` or ``feature/x`` → flat."""
    return (
        re.sub(
            r"[^A-Za-z0-9._-]+",
            "-",
            ref.replace("refs/heads/", "").replace("refs/tags/", ""),
        )
        or "ref"
    )


def export_ref(ref: str, dest: Path) -> None:
    """``git archive ref docs/install`` → dest (the docs/install tree itself)."""
    prefix = git(
        "rev-parse", "--show-prefix"
    )  # docs/install/ relative to the repo root
    top = Path(git("rev-parse", "--show-toplevel"))
    with tempfile.NamedTemporaryFile(suffix=".tar", delete=False) as tmp:
        archived = subprocess.run(
            [
                "git",
                "archive",
                "--format=tar",
                "-o",
                tmp.name,
                ref,
                "--",
                prefix.rstrip("/"),
            ],
            cwd=top,
            check=False,
            capture_output=True,
            text=True,
        )
        if archived.returncode:
            sys.exit(
                f"error: cannot export {prefix} from {ref}: {archived.stderr.strip()}"
            )
    try:
        with tarfile.open(tmp.name) as tar:
            members = [m for m in tar.getmembers() if m.name.startswith(prefix)]
            for m in members:
                m.name = m.name[len(prefix) :]
            members = [m for m in members if m.name]
            tar.extractall(dest, members=members, filter="data")
    finally:
        Path(tmp.name).unlink(missing_ok=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument(
        "refs", nargs="+", help="REF or REF=LABEL; the first is the default"
    )
    ap.add_argument(
        "--out",
        required=True,
        type=Path,
        help="site output directory (created; existing ref dirs replaced)",
    )
    ap.add_argument(
        "--allow-overlay",
        action="store_true",
        help="for a ref without docs/install/explorer, overlay this checkout's explorer",
    )
    ap.add_argument(
        "--python",
        default=sys.executable,
        help="python used to run mkdoc.py --manifest",
    )
    args = ap.parse_args()

    out: Path = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    versions = []
    for spec in args.refs:
        ref, _, label = spec.partition("=")
        label = label or ref
        git_ref = resolve_ref(ref)
        revision = git("rev-parse", git_ref)
        vc_date = git("log", "-1", "--pretty=format:%as", git_ref)
        name = safe_dir(
            ref if ref != "HEAD" else git("rev-parse", "--abbrev-ref", "HEAD")
        )
        dest = out / name
        if dest.exists():
            shutil.rmtree(dest)
        print(
            f"{ref} ({revision[:9]}, {vc_date}) -> {dest.relative_to(out.parent) if dest.is_relative_to(out.parent) else dest}"
        )
        export_ref(git_ref, dest)
        overlay = False
        if not (dest / "explorer" / "engine.js").exists():
            if not args.allow_overlay:
                print(
                    f"  error: {ref} has no docs/install/explorer; pass --allow-overlay to use this checkout's",
                    file=sys.stderr,
                )
                return 1
            shutil.copytree(
                HERE,
                dest / "explorer",
                ignore=shutil.ignore_patterns("manifest.json", "__pycache__"),
                dirs_exist_ok=True,
            )
            overlay = True
            print("  (explorer overlaid from this checkout)")
        stamp = ["--vc-revision", revision, "--vc-date", vc_date, "--vc-branch", ref]
        own = subprocess.run(
            [args.python, str(dest / "mkdoc.py"), "--manifest", *stamp],
            check=False,
            capture_output=True,
            text=True,
        )
        if own.returncode:
            # the ref's mkdoc.py predates --manifest: index its tree with ours
            print("  (manifest written by this checkout's mkdoc.py)")
            subprocess.run(
                [
                    args.python,
                    str(INSTALL / "mkdoc.py"),
                    "--manifest",
                    str(dest / "explorer" / "manifest.json"),
                    "--base",
                    str(dest),
                    *stamp,
                ],
                check=True,
            )
        versions.append(
            {
                "ref": ref,
                "label": label,
                "path": f"{name}/",
                "revision": revision,
                "date": vc_date,
                "explorer_overlaid": overlay,
            }
        )

    (out / "versions.json").write_text(
        json.dumps(
            {
                "generated": time.strftime("%Y-%m-%d", time.gmtime()),
                "default": versions[0]["path"],
                "versions": versions,
            },
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )
    (out / "index.html").write_text(
        f'<!doctype html><meta charset="utf-8"><meta http-equiv="refresh" content="0; url={versions[0]["path"]}explorer/">\n'
        f'<title>OpenHPC Recipe Explorer</title><p><a href="{versions[0]["path"]}explorer/">OpenHPC recipe explorer</a></p>\n',
        encoding="utf-8",
    )
    (out / ".nojekyll").touch()
    print(f"site: {out} ({len(versions)} version(s); default {versions[0]['ref']})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
