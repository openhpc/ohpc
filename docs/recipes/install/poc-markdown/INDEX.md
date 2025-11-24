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

### Common Templates (5 files)
1. `common/title.md.j2` - Title page
2. `common/enable_ohpc_repo.md.j2` - Repository setup
3. `common/install_slurm.md.j2` - Slurm installation
4. `common/inputs.md.j2` - Input variables documentation
5. `common/warewulf4_setup.md.j2` - Warewulf setup

### Supporting Infrastructure
- `common/generate_vc.sh` - Version control info extractor
- `Makefile` - Build orchestration
- `config.yaml` - All recipe variables

## Generated Output

- **steps.md**: 239 lines, 8.3KB
- Includes title, TOC, installation steps, code blocks, notes
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
├── common/
│   ├── title.md.j2
│   ├── enable_ohpc_repo.md.j2
│   ├── install_slurm.md.j2
│   ├── inputs.md.j2
│   ├── warewulf4_setup.md.j2
│   └── generate_vc.sh
└── rocky10/x86_64/warewulf4/slurm/
    ├── config.yaml          ← Recipe variables
    ├── steps.md.j2          ← Main template
    ├── Makefile             ← Build system
    └── steps.md             ← Generated output
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
