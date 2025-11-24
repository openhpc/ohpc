# Quick Start Guide

## Prerequisites

Ensure you have `jinja2` CLI tool installed. On Fedora/RHEL/Rocky:
```bash
dnf install python3-jinja2-cli
```

Or via pip:
```bash
pip install jinja2-cli[yaml]
```

## Try the POC

1. **Navigate to the recipe directory:**
   ```bash
   cd docs/recipes/install/poc-markdown/rocky10/x86_64/warewulf4/slurm
   ```

2. **Build the documentation:**
   ```bash
   make
   ```

3. **View the generated markdown:**
   ```bash
   cat steps.md
   # or
   make view
   ```

4. **Inspect the source files:**
   - `config.yaml` - All variables (OS, provisioner, package manager commands, etc.)
   - `steps.md.j2` - Main template structure
   - `../../../../common/*.md.j2` - Reusable content templates

5. **Try modifying variables:**
   ```bash
   # Edit config.yaml to change baseOS
   vim config.yaml
   # Rebuild
   make clean && make
   ```

6. **Generate PDF (optional):**
   ```bash
   make pdf
   # Requires pandoc: dnf install pandoc texlive-xetex
   ```

## Understanding the Files

### config.yaml
Contains all recipe-specific variables:
- OS configuration (baseOS, OSTree, arch)
- Package manager commands (install, upgrade, etc.)
- Provisioner and resource manager names
- Boolean toggles for conditional content
- Product names and URLs

### steps.md.j2
The main template that:
- Defines document structure
- Includes common templates with `{% include 'common/...' %}`
- Uses variables with `{{ variable }}`
- Has conditionals with `{% if condition %}...{% endif %}`

### common/*.md.j2
Reusable templates shared across all recipes:
- `enable_ohpc_repo.md.j2` - Repository setup
- `install_slurm.md.j2` - Slurm installation
- `inputs.md.j2` - Required variables documentation
- `warewulf4_setup.md.j2` - Warewulf configuration

### Makefile
Automates the build process:
- Creates symlink to common templates
- Runs jinja2 with --strict mode
- Cleans generated files
- Optional PDF generation

## Example Customizations

### Change the OS

Edit `config.yaml`:
```yaml
baseOS: "AlmaLinux 10"
OSTag: "el10"
baseos: "alma-10"
```

Rebuild:
```bash
make clean && make
```

### Add conditional content

In `steps.md.j2` or any template:
```markdown
{% if isx86 %}
This content only appears for x86_64 architecture.
{% endif %}

{% if isWarewulf4 %}
Warewulf 4 specific instructions...
{% else %}
Warewulf 3 instructions...
{% endif %}
```

### Add new variables

In `config.yaml`:
```yaml
my_custom_path: "/opt/custom"
```

Use in templates:
```markdown
Install to {{ my_custom_path }}
```

## Comparison with LaTeX

**Before (LaTeX):**
```latex
\newcommand{\baseOS}{Rocky 10}
Enable use of the \OHPC{} repository on \baseOS{}
```

**After (Jinja2):**
```yaml
baseOS: "Rocky 10"
OHPC: "OpenHPC"
```
```markdown
Enable use of the {{ OHPC }} repository on {{ baseOS }}
```

## Viewing the Output

The generated `steps.md` is a standard Markdown file that can be:
- Viewed in any text editor
- Rendered on GitHub/GitLab
- Converted to HTML with pandoc or MkDocs
- Converted to PDF with pandoc
- Viewed in a Markdown previewer

## Next Steps

1. Review the generated `steps.md`
2. Compare with the original LaTeX approach
3. Read `README.md` for architecture details
4. Read `COMPARISON.md` for detailed feature comparison
5. Experiment with creating your own templates

## Getting Help

- Jinja2 documentation: https://jinja.palletsprojects.com/
- jinja2-cli: https://github.com/mattrobenolt/jinja2-cli
- Markdown guide: https://www.markdownguide.org/
- Pandoc manual: https://pandoc.org/MANUAL.html
