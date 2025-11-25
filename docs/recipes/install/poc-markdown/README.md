# OpenHPC Documentation: Markdown + Jinja2 Proof-of-Concept

This directory contains a proof-of-concept for converting OpenHPC documentation from LaTeX to Markdown using Jinja2 templating.

## Overview

The current OpenHPC documentation uses:
- LaTeX for document formatting
- `\newcommand` macros for variables (OS name, package manager commands, etc.)
- `\input{}` directives to include common content
- `\iftoggleverb{}` for conditional compilation
- Special comment markers for script generation

This POC replaces that with:
- Markdown for document formatting
- YAML configuration files for variables
- Jinja2 templating for includes and conditionals
- HTML comments (`<!-- -->`) for script generation markers
- Command-line jinja2 tool + Makefiles for building

## Directory Structure

```
poc-markdown/
├── common/                          # Shared templates (like common/*.tex)
│   ├── enable_ohpc_repo.md.j2
│   ├── install_slurm.md.j2
│   ├── inputs.md.j2
│   ├── title.md.j2                 # Uses HTML styling, not headings
│   ├── warewulf4_setup.md.j2
│   ├── generate_vc.sh              # Script to generate version control YAML
│   └── update-toc.sh               # Script to generate TOC with mdtoc
└── rocky10/x86_64/warewulf4/slurm/  # Recipe-specific files
    ├── config.yaml                  # Variables (replaces LaTeX macros)
    ├── steps.md.j2                  # Main template (replaces steps.tex)
    ├── Makefile                     # Build system
    ├── common -> ../../../../common # Symlink for includes
    ├── vc.yaml                      # Generated version control info
    ├── .config.merged.yaml          # Merged config (generated)
    └── steps.md                     # Generated output
```

## Key Mappings

| LaTeX Feature | Jinja2/Markdown Equivalent |
|---------------|---------------------------|
| `\newcommand{\baseOS}{Rocky 10}` | `baseOS: "Rocky 10"` in config.yaml |
| `\baseOS{}` | `{{ baseOS }}` |
| `\install{}` | `{{ install }}` |
| `\iftoggleverb{isCentOS}...\fi` | `{% if isCentOS %}...{% endif %}` |
| `\input{common/install_slurm}` | `{% include 'common/install_slurm.md.j2' %}` |
| LaTeX code blocks | Markdown code blocks with triple backticks |
| `\texttt{}` | Inline code with backticks |
| `\textbf{}` | Bold with `**text**` or `*text*` |
| Title page headings | `<p style="font-size: Xpx;">` (avoids chapter numbers) |
| `% begin_ohpc_run` | `<!-- begin_ohpc_run -->` |
| `% ohpc_command` | `<!-- ohpc_command ... -->` |
| TOC generation | `mdtoc` with `<!-- toc -->` markers |

## Building

From the recipe directory:

```bash
cd rocky10/x86_64/warewulf4/slurm
make              # Build steps.md from templates
make view         # View the generated markdown
make clean        # Remove generated files
make pdf          # Generate PDF (requires pandoc)
```

## How It Works

1. **Makefile** creates a symlink to `common/` directory for template includes
2. **generate_vc.sh** extracts git revision and date, writes to `vc.yaml`
3. **Makefile** merges `config.yaml` and `vc.yaml` into `.config.merged.yaml`
4. **jinja2** CLI processes `steps.md.j2` with variables from merged config
5. Jinja2 `{% include %}` directives pull in common templates (including `title.md.j2`)
6. Variables like `{{ baseOS }}`, `{{ VCRevision }}` are substituted
7. Conditionals like `{% if isCentOS %}` control content
8. Output is written to `steps.md`
9. **update-toc.sh** runs `mdtoc` to generate table of contents between `<!-- toc -->` markers

## Benefits

1. **Human-readable source**: Markdown is easier to read/write than LaTeX
2. **Simpler syntax**: No LaTeX escaping needed for special characters
3. **Better Git diffs**: Plain text Markdown shows clearer changes
4. **Multiple output formats**: Markdown can convert to HTML, PDF, etc.
5. **Standard tooling**: Jinja2 is widely used, well-documented
6. **Makefile-based**: Fits existing build infrastructure

## Version Control Integration

The build system automatically extracts git information:

- **generate_vc.sh** runs `git log` to get current revision and date
- Outputs to `vc.yaml` which is merged with `config.yaml`
- Title page displays: `VCRevision` (git commit hash) and `VCDateISO` (commit date)
- Works similar to LaTeX `vc` script but generates YAML instead of .tex

This mirrors the current LaTeX system where `common/vc` generates `vc.tex`.

## Script Generation

The special HTML comment markers like `<!-- begin_ohpc_run -->` and `<!-- ohpc_command -->`
are preserved in the Markdown output and are post-processed by `parse_doc_md.py` to generate
installation scripts (recipe.sh), similar to the current LaTeX system.

## Next Steps for Full Implementation

1. Convert all remaining common/*.tex files to Markdown templates
2. Create config.yaml for all recipe variants
3. Create script to post-process script generation markers
4. Set up CI/CD to validate all recipes build correctly
5. Create conversion scripts/tools for bulk migration
6. Document the new system for contributors
7. Consider using MkDocs or similar for web documentation

## Requirements

- jinja2-cli: `pip install jinja2-cli[yaml]`
- (Optional) pandoc for PDF generation
