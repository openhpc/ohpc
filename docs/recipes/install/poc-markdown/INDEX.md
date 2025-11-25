# OpenHPC Markdown+Jinja2 Documentation POC - Index

## Quick Links

- **[QUICKSTART.md](QUICKSTART.md)** - Start here! Try the POC in 5 minutes
- **[README.md](README.md)** - Architecture and detailed overview
- **[COMPARISON.md](COMPARISON.md)** - Side-by-side LaTeX vs Markdown comparison
- **[SUMMARY.md](SUMMARY.md)** - Complete summary of what was built
- **[PDF_GENERATION.md](PDF_GENERATION.md)** - PDF generation guide with Pandoc
- **[TOC_GENERATION.md](TOC_GENERATION.md)** - Table of contents generation with mdtoc

## What Is This?

A proof-of-concept demonstrating conversion of OpenHPC installation documentation from LaTeX to Markdown using Jinja2 templating.

## Current Status: ✅ FULLY FUNCTIONAL

The POC successfully builds complete documentation with:
- Title page with version control info
- Variable substitution and conditionals
- File includes from common templates
- Script generation markers preserved
- Makefile-based build system

## Try It Now

```bash
cd docs/recipes/install/poc-markdown/rocky10/x86_64/warewulf4/slurm
make
cat steps.md
```

## What Was Converted

### Common Templates (74 files)
The POC has been expanded to include comprehensive coverage:
- Title and introduction templates
- Repository and provisioning setup
- Resource manager installation (Slurm)
- Development tools, compilers, MPI stacks
- Performance tools and libraries
- Network configuration (InfiniBand, Omni-Path)
- Customization and optional components
- Package manifest templates

### Supporting Infrastructure
- `common/generate_vc.sh` - Version control info extractor
- `common/parse_doc_md.py` - Recipe script generator
- `common/update-toc.sh` - Table of contents generator
- `Makefile` - Build orchestration with PDF/HTML support
- `config.yaml` - All recipe variables
- Lua filters and CSS for format-specific rendering

## Generated Output

- **steps.md**: 2817 lines, 122KB
- **steps.pdf**: 259KB (44 pages)
- **steps.html**: 222KB
- **recipe.sh**: 517 lines (executable installation script)
- Includes title, TOC, installation steps, code blocks, tips
- All variables substituted correctly
- Conditionals render properly
- Includes resolved successfully

## Key Features

✅ Simpler syntax (~50% fewer lines)
✅ Readable YAML configuration
✅ Standard tooling (jinja2, make)
✅ Git-friendly source files
✅ Multiple output formats possible
✅ Maintains existing workflow

## File Structure

```
poc-markdown/
├── INDEX.md                  ← You are here
├── QUICKSTART.md            ← Start here for hands-on
├── README.md                ← Architecture details
├── COMPARISON.md            ← LaTeX vs Markdown
├── SUMMARY.md               ← What was built
├── PDF_GENERATION.md        ← PDF generation guide
├── TOC_GENERATION.md        ← TOC generation guide
├── common/                  ← 74 shared Jinja2 templates
│   ├── *.md.j2              ← Content templates
│   ├── generate_vc.sh       ← Version control extractor
│   ├── parse_doc_md.py      ← Recipe script generator
│   ├── update-toc.sh        ← TOC generator
│   ├── header-includes.tex.j2 ← PDF header template
│   ├── format-filters.lua   ← Pandoc Lua filter
│   └── codeblock-styles.css ← HTML styling
├── rocky10/x86_64/warewulf4/slurm/
│   ├── config.yaml          ← Recipe variables
│   ├── steps.md.j2          ← Main template
│   ├── manifest.md.j2       ← Package manifest
│   ├── manifest/*.md.j2     ← Package tables
│   ├── Makefile             ← Build system
│   ├── steps.md             ← Generated markdown
│   ├── steps.pdf            ← Generated PDF
│   ├── steps.html           ← Generated HTML
│   └── recipe.sh            ← Generated install script
└── almalinux10/x86_64/warewulf4/slurm/
    └── (same structure as rocky10)
```

## Documentation Guide

1. **New to the project?** → Read [QUICKSTART.md](QUICKSTART.md)
2. **Want architecture details?** → Read [README.md](README.md)
3. **Comparing LaTeX vs Markdown?** → Read [COMPARISON.md](COMPARISON.md)
4. **Want complete summary?** → Read [SUMMARY.md](SUMMARY.md)

## Prerequisites

- `jinja2` CLI tool (`dnf install python3-jinja2-cli`)
- `make`
- `git` (for version control info)
- (Optional) `pandoc` for PDF generation

## Questions?

See the individual documentation files listed above for detailed information about:
- How to use the system
- How it works
- What's different from LaTeX
- Implementation details
- Migration path

## Bottom Line

**This POC proves that converting OpenHPC documentation to Markdown+Jinja2 is feasible, beneficial, and ready for broader implementation.**
