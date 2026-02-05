# OpenHPC Documentation System Design

This document captures the design of the Markdown-based documentation system
for OpenHPC installation recipes.

## Goals

- Make documentation easier to edit and contribute to
- Normalize variable names and remove duplication
- Support multiple provisioners (Warewulf, OpenCHAMI, Confluent) and schedulers (Slurm)
- Support multiple distros (Rocky, AlmaLinux, openEuler, SLES) and
  architectures (x86_64, aarch64)
- Generate installation scripts directly from documentation
- Single Python build tool with Makefile orchestration (no shell script pipelines)

## Architecture Overview

### Recipe = .conf + .yaml Pair

A recipe is defined by two files in `recipes/`:

- **`*.conf`** — ordered list of `config/` YAML files to merge
- **`*.yaml`** — per-recipe overrides (Confluent only; omitted for other recipes)

The Makefile merges these into a single `build/*.yaml` using `yq` deep
merge, which mkdoc.py then reads as its input.

```text
# recipes/rocky10-x86_64-warewulf-slurm.conf
config/base.yaml
config/distro/el10.yaml
config/distro/rocky.yaml
config/arch/x86_64.yaml
config/provisioner/warewulf.yaml
config/scheduler/slurm.yaml
```

Confluent recipes additionally have a `.yaml` with two per-combination
overrides that cannot be derived from the config hierarchy:

```yaml
# recipes/rocky10-x86_64-confluent-slurm.yaml
distro_id: "rocky-10.1-x86_64-default"
distro_iso_image: "Rocky-10.1-x86_64-dvd1.iso"
```

All document composition is handled by `templates/chapters/main.md.j2` —
recipes contain only config declarations. Each chapter aggregator is
self-contained and handles distro-specific includes via conditionals.

### Config Inheritance

Configs are merged in order: base → distro family → distro → arch →
provisioner → scheduler → recipe overrides. Only one version of each OS
is supported at a time.

```text
config/
├── base.yaml                    # ohpc_version, boolean flag defaults
├── distro/
│   ├── el10.yaml                # EL10 family: version + pkg commands + is_el: true
│   ├── rocky.yaml               # Rocky: just name + image
│   ├── almalinux.yaml           # AlmaLinux: just name + image
│   └── openeuler.yaml           # openEuler: own pkg commands + distro info
├── arch/
│   ├── x86_64.yaml              # is_x86_64: true
│   └── aarch64.yaml             # is_aarch64: true
├── provisioner/
│   ├── warewulf.yaml            # is_warewulf: true, provisioner_name: "Warewulf"
│   ├── openchami.yaml           # is_openchami: true, provisioner_name: "OpenCHAMI"
│   └── confluent.yaml           # is_confluent: true, provisioner_name: "Confluent"
└── scheduler/
    └── slurm.yaml               # is_slurm: true, scheduler_name: "Slurm"
```

Boolean flags (e.g., `is_x86_64`, `is_warewulf`, `is_el`) default to `false` in
`base.yaml` and are set to `true` in the specific config file. Templates use
these flags for conditional content without runtime computation.

### Composable Sections with Aggregator Templates

`templates/chapters/main.md.j2` is the single document entry point —
it includes all 12 chapter aggregators, with two provisioner-specific
chapters resolved dynamically at render time via `~ provisioner ~`
string concatenation. Aggregator templates handle the inclusion of
detailed subsections with conditionals.

The provisioner-specific chapters (`provisioner-*` and `deploy-*`) differ because
provisioners have fundamentally different workflows:

- **Warewulf**: build chroot image → customize chroot → boot nodes
- **Confluent**: boot nodes from Confluent → configure live nodes via nodeshell
- **OpenCHAMI**: build layered container image (podman + yq) → cloud-init →
  boot nodes

Aggregator templates use `{% include %}` to compose sections:

```jinja2
{# chapters/dev-tools.md.j2 #}
# Install OpenHPC Development Components
{% include "dev-tools/intro.md.j2" %}
{% include "dev-tools/compilers.md.j2" %}
{% include "scheduler/slurm/mpi.md.j2" %}
{% include "dev-tools/perf-tools.md.j2" %}
{% if is_x86_64 %}
{% include "dev-tools/third-party-mpi-libs-x86.md.j2" %}
{% endif %}
```

Distro-specific content (e.g., `distro/rocky/repos.md.j2`) is included
inside workflow aggregators with `{% if distro_name == "Rocky" %}` conditionals.

### Heading Ownership

Headings follow consistent ownership rules:

- **`#` chapter** — in chapter aggregators
- **`##` section** — in template files (self-contained)
- **`###` grouping** — in aggregators (groups multiple includes)
- **`####` leaf** — in leaf template files

Each `##`-level template carries its own heading, making templates self-describing
and independently readable. Aggregators contain only `#` chapter headings,
`###` grouping headers, and `{% include %}` directives — no `##` headings.
Filenames are self-documenting, so aggregators do not use `{# Comment #}` lines
that merely restate the filename. Chapter separator comments
(`{# ===== Chapter: ... ===== #}`) are used for readability in multi-chapter
workflows.

### Chapter Reference

The 12 chapters form a pipeline; each assumes the previous ones are complete.
When adding content, choose the chapter whose preconditions match.

**front-matter** — Title page, legal, table of contents.

**introduction** — Overview, requirements, and input variables.

**base-os** — Head node setup: epel, ohpc repos, ohpc-base packages, firewall,
time sync, NFS, networking. InfiniBand and OmniPath server-side installed here
(optional). Provisioners (notably Warewulf) handle parts of this themselves.

**ohpc** — OpenHPC repository and base packages on the head node.

**scheduler-slurm** — Slurm installed on the head node only. Compute-side Slurm
configuration goes in `provisioner-*`; Slurm startup goes in `deploy-*`. Adding
a second scheduler would require refactoring this split.

**provisioner-*** — Provisioner fully installed; base compute node, image, or
definition created with: epel, repos, ohpc-compute, kernel, firewall disabled,
NFS mounts. InfiniBand/OmniPath compute-side excluded here (goes in `customize`).

**customize** — Compute image additions. Head node, provisioner, and scheduler
are configured; compute nodes may or may not be running. Use `compute_*` macros
for provisioner-agnostic operations (see [Macro System](#macro-system)).
InfiniBand and OmniPath compute-side go here.

**deploy-*** — Cluster booted; compute nodes provisioned; Slurm started. Scope:
maintenance-window actions (adding/removing nodes). Most provisioners boot here;
Confluent boots during `provisioner-confluent`.

**dev-tools** — Login-node development tools: compilers, MPI, performance tools,
third-party libraries.

**test** — Cluster in production. Test job submission.

**post** — Cluster in production. Routine admin actions (users, monitoring).

**appendices** — Supplementary topics. Appendix chapters use `#` headings — they
are top-level document sections, not `##` subsections.

### Handling Provisioner Differences

Five strategies handle differences between provisioners in shared sections:

| Strategy | When to use | Example |
| -------- | ----------- | ------- |
| **Config override** | Variable-driven commands | `confluent.yaml` sets `pkg_install_chroot` to `nodeshell compute dnf -y install` |
| **Macro abstraction** | Compute image ops (install, sed, echo, run) | `{{ compute_sed(regex, file) }}` dispatches to chroot/nodeshell/yq |
| **Variant template** | Fundamentally different structure | `provisioner/openchami/memlimits.md.j2` vs warewulf equivalent |
| **Conditional block** | Small inline differences | `{% if is_openchami %}...{% elif is_confluent %}...{% endif %}` |
| **Convention-based include** | Per-provisioner file with same name | `{% include "provisioner/" ~ provisioner ~ "/test-job.md.j2" %}` |

The config override strategy is the most powerful — it lets templates like
`network/infiniband/compute-node.md.j2` work across provisioners without
changes, because they use `{{ pkg_install_chroot }}` rather than hardcoded
commands. Macro abstraction (see below) extends this to structural operations
on the compute image.

### Macro System

Global macros are defined in `templates/macros.j2` and loaded by `mkdoc.py`
before template rendering. All config variables (`is_warewulf`, `provisioner`,
`pkg_install`, etc.) are available in `macros.j2` at load time.

The macro override pattern: a default macro is defined first, then
provisioner-specific blocks redefine it. Jinja2's last-definition-wins
rule ensures the correct version is active for each recipe.

```jinja2
{# Default (Warewulf chroot) #}
{%- macro compute_install(packages) -%}
{{ pkg_install_chroot }} {{ packages | join(' ') }}
{%- endmacro -%}

{# OpenCHAMI override #}
{% if is_openchami %}
{%- macro compute_install(packages) -%}
yq -i '.packages += {{ packages | tojson }}' \
    /opt/ohpc/admin/images/compute-prod.yaml
{%- endmacro -%}
{% endif %}
```

#### Compute Image Macros

These four macros abstract all provisioner differences for compute image
operations. Templates use them without knowing which provisioner is active:

| Macro | Warewulf | Confluent | OpenCHAMI |
| ----- | -------- | --------- | --------- |
| `compute_install(packages)` | `dnf install` in chroot | `nodeshell compute dnf install` | `yq` append to packages array |
| `compute_sed(regex, file)` | `sed -i` on `$CHROOT/file` | `nodeshell compute sed -i` | `yq` append to cmds array |
| `compute_echo(string, file)` | `echo` to `$CHROOT/file` | `nodeshell compute echo` | `yq` append to cmds array |
| `compute_run(cmd)` | `wwctl image exec` | `nodeshell compute` | `yq` append to cmds array |

`head_install(packages)` installs packages on the head node (uses
`pkg_install`, consistent across provisioners).

**Usage rules** (learned from debugging):

- `compute_run(cmd)` — use for any non-package command: enabling repos
  (`/usr/bin/crb enable`), restarting services, or writing files that need
  runtime variable expansion (e.g. `${sms_ip}`). For the latter, use a
  single-quoted echo: `compute_run("echo '${var}' > /path")` — the outer
  `"..."` expands the variable on the head node before passing to the
  provisioner.
- `compute_echo(string, file)` — `string` is the **content only**, never
  include `echo` in the string. Use `$HOME` not `~` (tilde does not expand
  inside double-quoted shell strings).
- `compute_sed(regex, file)` — regex in single quotes, absolute file path
  without `$CHROOT` prefix (macros add it for Warewulf).

#### Script Extraction Markers

HTML comment markers in `.md.j2` templates control what `mkdoc.py` extracts
into `recipe.sh`. All markers follow the `ohpc_` prefix. They are invisible
in rendered markdown (HTML comments) and do not appear in the PDF or HTML output.

**Block delimiters** — wrap all extractable content:

```markdown
<!-- ohpc_begin -->
... extractable content ...
<!-- ohpc_end -->
```

**Within a block:**

| Marker | Extracted as |
| --- | --- |
| `<!-- ohpc_command CMD -->` | `CMD` (raw shell line) |
| `<!-- ohpc_comment TEXT -->` | `# TEXT` (shell comment) |
| `<!-- ohpc_if_set VAR -->` | `if [[ ${VAR} -eq 1 ]];then` |
| `<!-- ohpc_if CONDITION -->` | `if CONDITION;then` |
| `<!-- ohpc_else -->` | `else` |
| `<!-- ohpc_fi -->` | `fi` |

Blank lines inside `ohpc_begin`/`ohpc_end` blocks pass through to the script.
Bash fenced code blocks (` ```bash `) inside a block are extracted verbatim.

Use `ohpc_if_set VAR` for all `enable_*` boolean flag conditionals (the common
case). Use `ohpc_if CONDITION` for complex cases: AND conditions, numeric
comparisons, file tests, command pipelines.

**Example — simple conditional:**

````markdown
<!-- ohpc_begin -->
<!-- ohpc_if_set enable_ib -->
```bash
dnf -y groupinstall "InfiniBand Support"
```
<!-- ohpc_fi -->
<!-- ohpc_end -->
````

**Example — pure-script block (no visible markdown content):**

```markdown
<!-- ohpc_begin -->
<!-- ohpc_if [ ! -e "${inputFile}" ] -->
<!-- ohpc_command    echo "Error: file not found" -->
<!-- ohpc_command    exit 1 -->
<!-- ohpc_else -->
<!-- ohpc_command    . "${inputFile}" -->
<!-- ohpc_fi -->
<!-- ohpc_end -->
```

**Runtime vs. build-time conditionals:** `ohpc_if_set`/`ohpc_if` produce
shell conditionals evaluated at install time (controlled by `input.local`
variables). Jinja2 `{% if ... %}` controls what appears in the document
at build time (controlled by recipe YAML flags). Use both as appropriate.

## Directory Structure

```text
docs/install/
├── mkdoc.py                     # Main build tool
├── generate_manifest.py         # Package manifest generator
├── pandoc/                      # Pandoc support files (PDF + HTML)
│   ├── header-includes.tex.j2   # LaTeX header template (page headers, code styling)
│   ├── format-filters.lua       # Lua filter (title fonts, tip boxes, format-variant images)
│   └── codeblock-styles.css     # CSS for HTML output (code blocks)
├── config/                      # Configuration hierarchy
│   ├── base.yaml
│   ├── distro/
│   ├── arch/
│   ├── provisioner/
│   └── scheduler/
├── templates/                    # Jinja2 templates
│   ├── macros.j2                # Global macros (loaded by mkdoc.py)
│   ├── chapters/                # Document entry point + chapter aggregators
│   │   ├── main.md.j2
│   │   ├── front-matter.md.j2
│   │   ├── introduction.md.j2
│   │   ├── base-os.md.j2
│   │   ├── ohpc.md.j2
│   │   ├── scheduler-slurm.md.j2
│   │   ├── provisioner-warewulf.md.j2
│   │   ├── provisioner-confluent.md.j2
│   │   ├── provisioner-openchami.md.j2
│   │   ├── customize.md.j2
│   │   ├── deploy-warewulf.md.j2
│   │   ├── deploy-confluent.md.j2
│   │   ├── deploy-openchami.md.j2
│   │   ├── dev-tools.md.j2
│   │   ├── test.md.j2
│   │   ├── post.md.j2
│   │   └── appendices.md.j2
│   ├── front-matter/
│   ├── intro/
│   ├── base-os/
│   ├── ohpc/
│   ├── provisioner/
│   │   ├── warewulf/
│   │   ├── confluent/
│   │   └── openchami/
│   ├── scheduler/
│   │   └── slurm/
│   ├── network/
│   │   ├── infiniband/
│   │   └── omnipath/
│   ├── optional/
│   ├── dev-tools/
│   ├── appendices/
│   ├── distro/
│   │   ├── rocky/
│   │   ├── almalinux/
│   │   └── openeuler/
│   └── figures/
├── manifests/                   # Package manifest data and config
│   ├── config.yaml              # Manifest generation config
│   ├── el10-x86_64/             # Named by distro_tag + arch
│   │   ├── pkg-ohpc.all         # Package data (from dnf)
│   │   ├── meta-ohpc.all        # Meta-package data (from dnf)
│   │   ├── pkg-ohpc.md.j2       # Generated package tables
│   │   └── meta-ohpc.md         # Generated meta-package table
│   ├── el10-aarch64/
│   ├── oe2403-x86_64/
│   └── oe2403-aarch64/
├── recipes/                     # Recipe YAML files (source only)
│   ├── rocky10-x86_64-warewulf-slurm.yaml
│   ├── almalinux10-x86_64-warewulf-slurm.yaml
│   └── ...
└── build/                       # Generated output (gitignored)
    ├── header-includes.tex      # Rendered from pandoc/header-includes.tex.j2
    ├── graphicspath.tex         # Generated absolute paths for xelatex
    ├── rocky10-x86_64-warewulf-slurm.md
    ├── rocky10-x86_64-warewulf-slurm.sh
    ├── rocky10-x86_64-warewulf-slurm.pdf
    └── rocky10-x86_64-warewulf-slurm.html
```

## Naming Conventions

### Files and Directories

- Lowercase with hyphens: `import-files.md.j2` not `import_ww4_files.md.j2`
- No numbered prefixes on directories (ordering is defined by recipes/aggregators)
- `.md.j2` extension for Jinja2 templates that produce Markdown

### Terminology

| Old Term | New Term | Notes |
| -------- | -------- | ----- |
| `bos`, `BOS` | `base-os`, `Base OS` | Base Operating System |
| `sms`, `SMS` | `head node` | Variables keep `sms_` prefix |
| `master` | `head node` | All prose updated |
| `[sms]#` prompt | *(removed)* | Commands shown without prompt |
| `warewulf4` | `warewulf` | v3 not supported, drop the version |
| `CentOS`, `centos` | `el` | Enterprise Linux family |
| `ib` | `infiniband` | No abbreviations |
| `opa` | `omnipath` | No abbreviations |

### Variables

- Use `snake_case` for all variable names
- Prefix package commands with `pkg_`: `pkg_install`, `pkg_clean`
- Prefix distro info with `distro_`: `distro_name`, `distro_version`
- Prefix feature flags with `enable_`: `enable_infiniband`, `enable_clustershell`
- Prefix boolean type flags with `is_`: `is_x86_64`, `is_warewulf`, `is_el`
- Capability flags: `uses_dnf` (set in distro configs, used instead of
  `is_el or is_openeuler`)
- Display names: `provisioner_name`, `scheduler_name` (used in page
  headers and titles)

## Build Tools

### mkdoc.py

Pure Jinja2 renderer and script extractor. Reads a pre-merged config YAML
(produced by the Makefile) and renders `templates/chapters/main.md.j2`:

```bash
mkdoc.py build/name.yaml                          # Generate .md in build/
mkdoc.py build/name.yaml --markdown output.md     # Generate .md at custom path
mkdoc.py build/name.yaml --with-script            # Also generate .sh in build/
mkdoc.py build/name.yaml --script output.sh       # Generate .sh at custom path
mkdoc.py build/name.yaml --list-vars              # Show all merged variables
mkdoc.py build/name.yaml --ignore-warnings        # Don't fail on warnings
```

PDF and HTML generation are handled by the Makefile via pandoc — use
`make pdf` and `make html`. The `pandoc/` directory contains:
- `header-includes.tex.j2` — Jinja2 template for the LaTeX preamble (page headers
  with recipe info, code block styling, tip box environment). Rendered with recipe
  config variables at build time.
- `format-filters.lua` — Pandoc Lua filter for title page fonts, tip boxes,
  right-aligned images, format-variant images, and checkmark conversion.
  For HTML, searches resource paths for `.svg` then `.png` fallback.
  For LaTeX, selects `.pdf` versions.
- `codeblock-styles.css` — CSS for HTML output (code block styling).

HTML output uses `--embed-resources` to produce self-contained files with
images embedded as base64 data URIs. All images use the `.format-variant`
class so the Lua filter selects the appropriate format (PDF for LaTeX,
SVG for HTML). SVG versions were converted from the PDF sources using
inkscape.

A `build/graphicspath.tex` is generated with absolute paths so xelatex can find
images from raw `\includegraphics` calls (pandoc's `--resource-path` does not
propagate to the xelatex subprocess).

### generate_manifest.py

Generates package manifest tables from `.all` data files:

```bash
# Generate markdown tables from existing .all files
generate_manifest.py manifests/el10-x86_64

# Query dnf to regenerate .all files first (requires target system)
generate_manifest.py manifests/el10-x86_64 --generate-all --version 4.0
lima python3 generate_manifest.py manifests/el10-aarch64 --generate-all --version 4.0
```

The `--generate-all` flag queries specific OpenHPC version repositories
(not system repos), filters by architecture, and applies arch-specific
exclusions. Configuration lives in `manifests/config.yaml`.

### Makefile

Top-level Makefile for building all recipes:

```bash
make                                              # Build all recipes (.md + .sh)
make script                                       # Build all recipe scripts (.sh)
make pdf                                          # Build all recipes with PDFs
make html                                         # Build all recipes as HTML
make check                                        # Run shellcheck on build/*.sh
make clean                                        # Remove generated files
make build/rocky10-x86_64-warewulf-slurm.md       # Build single recipe
make build/rocky10-x86_64-warewulf-slurm.sh       # Build single script
make build/rocky10-x86_64-warewulf-slurm.pdf      # Build single PDF
```

Override with `make PYTHON=python3` to use system packages instead
(used by RPM builds). Uses coarse-grained dependencies (rebuild recipe
if any source changes). Individual builds are fast (~0.3s), so this is
simple and correct.

The Makefile injects `vc_revision` and `vc_date` (from `git log`) into
each `build/*.yaml` via `yq` during the merge step, so mkdoc.py needs
no subprocess calls. The `.yaml` prerequisite for `build/%.yaml` is
optional via `.SECONDEXPANSION` — only Confluent recipes have one.

### RPM Packaging

The `docs-ohpc` RPM is built from `components/admin/docs/`:

```text
components/admin/docs/
├── SOURCES/
│   ├── get_source.sh     # Creates docs-ohpc.tar from repo
│   ├── OHPC_macros       # Symlink to ../../OHPC_macros
│   └── rpmlintrc
└── SPECS/
    └── docs.spec
```

`get_source.sh` creates a tarball containing `.git/`, `docs/install/`,
`docs/ChangeLog`, and `docs/Release_Notes.txt`. The spec builds all recipes
via `make PYTHON=python3` and installs them to:

```text
/opt/ohpc/pub/doc/recipes/{distro}/{arch}/{provisioner}/{scheduler}/
├── Install_guide.pdf
├── Install_guide.md
├── Install_guide.html
└── recipe.sh
```

Plus `input.local` per distro (from `docs/install/input.local.template`).

Build with:
```bash
cd components/admin/docs/SOURCES && bash get_source.sh
rpmbuild -ba ../SPECS/docs.spec
# or via CI:
python3 tests/ci/run_build.py $USER components/admin/docs/SPECS/docs.spec
```

## Reference

### Recipe Naming

Recipes are named `{distro}{version}-{arch}-{provisioner}-{scheduler}.yaml`
and live in `recipes/`. See existing recipes for examples. The 14 current
recipes cover Warewulf, Confluent, and OpenCHAMI across Rocky, AlmaLinux,
and openEuler on x86\_64 and aarch64.

### Manifest Directory Naming

Manifest directories are named `{distro_tag}-{arch}` to match the lookup
pattern in `manifest.md.j2`:
`{% include distro_tag ~ '-' ~ arch ~ '/meta-ohpc.md' %}`.
When adding a new distro, the manifest directory name must match the
`distro_tag` value in the distro config (e.g., `oe2403` not
`openeuler24.03`).
