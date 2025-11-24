# Table of Contents Generation with mdtoc

## Overview

The documentation uses `mdtoc` (Markdown Table of Contents generator) to automatically create and maintain a table of contents in the generated Markdown files. This is inspired by the Kubernetes enhancements repository approach.

## How It Works

1. **Jinja2 template** (`steps.md.j2`) includes TOC markers:
   ```markdown
   <!-- toc -->
   <!-- /toc -->
   ```

2. **Makefile** generates `steps.md` from the template

3. **update-toc.sh** script runs `mdtoc` to populate the TOC between the markers

4. **mdtoc** scans all headings and generates clickable links

## Files Involved

### common/update-toc.sh
Script that runs `mdtoc` on markdown files:
```bash
#!/usr/bin/env bash
mdtoc --inplace --max-depth=3 "$file"
```

### steps.md.j2
Template includes TOC markers after the title:
```markdown
{% include 'common/title.md.j2' %}

<!-- toc -->
<!-- /toc -->

## Introduction
...
```

### Makefile
Runs update-toc.sh after generating markdown:
```makefile
$(OUTPUT): $(TEMPLATE) $(MERGED_CONFIG) $(wildcard $(COMMON_TEMPLATES)/*.md.j2)
	$(JINJA2) $(JINJA2_FLAGS) $(TEMPLATE) $(MERGED_CONFIG) -o $(OUTPUT)
	$(COMMON_TEMPLATES)/update-toc.sh $(OUTPUT)
```

## mdtoc Options

The `update-toc.sh` script uses:

- `--inplace`: Edit the file in-place (requires toc markers)
- `--max-depth=3`: Include headings up to level 3 (###)
- `--skip-prefix`: Skip headers before TOC markers (default true)

## Generated TOC Format

mdtoc generates a nested list with clickable links:

```markdown
<!-- toc -->
- [Introduction](#introduction)
  - [Inputs](#inputs)
- [Install Base Operating System (BOS)](#install-base-operating-system-bos)
- [Install OpenHPC Components](#install-openhpc-components)
  - [Enable OpenHPC repository for local use](#enable-openhpc-repository-for-local-use)
  - [Add resource management services on *master* node](#add-resource-management-services-on-master-node)
  - [Complete basic Warewulf setup for *master* node](#complete-basic-warewulf-setup-for-master-node)
- [Resource Manager Startup](#resource-manager-startup)
- [Run a Test Job](#run-a-test-job)
<!-- /toc -->
```

**Heading hierarchy:**
- Top-level sections (Introduction, Install Base Operating System, etc.) use `#` (H1)
- Subsections (Inputs, Enable repository, etc.) use `##` (H2)
- This creates proper chapter numbering in PDF (1, 1.1, 2, 2.1, etc.)

## Anchor Generation

mdtoc automatically generates GitHub-flavored anchors:

| Heading | Anchor |
|---------|--------|
| `# Introduction` | `#introduction` |
| `# Install Base Operating System (BOS)` | `#install-base-operating-system-bos` |
| `# Run a Test Job` | `#run-a-test-job` |
| `## Add resource management services` | `#add-resource-management-services` |

**Rules:**
- Convert to lowercase
- Replace spaces with hyphens
- Remove special characters (keep letters, numbers, hyphens)
- Parentheses content is included

## Important Notes

### ❌ Avoid Horizontal Rules Before Headings

**Problem:** mdtoc has a bug where horizontal rules (`---`) before headings can cause those headings to be skipped from the TOC.

**Example that causes issues:**
```markdown
---

# Some Section

This section will be MISSING from the TOC!
```

**Solution:** Don't use `---` separators between sections. Use blank lines instead:
```markdown
# Previous Section

# Some Section

This section will appear correctly in the TOC.
```

### ❌ Don't Use HTML Anchors

**Problem:** HTML anchor tags appear in the TOC text.

**Bad:**
```markdown
# Introduction <a name="introduction"></a>
```

Generates:
```markdown
- [Introduction <a name="introduction"></a>](#introduction-)
```

**Good:**
```markdown
# Introduction
```

Generates:
```markdown
- [Introduction](#introduction)
```

mdtoc generates anchors automatically - no manual anchors needed!

### Skip Headers Before TOC

By default, mdtoc skips any headings that appear before the `<!-- toc -->` marker. This is controlled by the `--skip-prefix` flag (default: true).

**Important:** The title page uses HTML `<p>` tags with inline styles instead of markdown headings. This has two benefits:

1. **No chapter numbering**: When Pandoc's `--number-sections` is enabled, it won't add numbers to title page text
2. **Not in TOC**: HTML paragraphs don't appear in the table of contents

Example:
```markdown
<!-- Title page uses HTML, not headings -->
<p style="font-size: 32px; font-weight: bold;">OpenHPC (v4.0)</p>
<p style="font-size: 24px; font-weight: bold;">Cluster Building Recipes</p>

<!-- toc -->
<!-- /toc -->

# Introduction  ← This WILL be in TOC (first real heading, level 1)
## Inputs       ← This WILL be in TOC (subsection, level 2)
# Installation  ← This WILL be in TOC (level 1)
```

The title content won't appear in the TOC because it's not using markdown heading syntax.

## Build Process

```bash
# Clean build
make clean && make

# This runs:
# 1. generate_vc.sh vc.yaml
# 2. cat config.yaml vc.yaml > .config.merged.yaml
# 3. jinja2 steps.md.j2 .config.merged.yaml -o steps.md
# 4. update-toc.sh steps.md  ← Generates TOC
```

The TOC is automatically updated every time you build.

## Manually Updating TOC

If you modify `steps.md` manually and want to regenerate the TOC:

```bash
../../../../common/update-toc.sh steps.md
```

Or using mdtoc directly:

```bash
mdtoc --inplace --max-depth=3 steps.md
```

## Comparison with LaTeX

| Feature | LaTeX | Markdown+mdtoc |
|---------|-------|----------------|
| TOC Generation | `\tableofcontents` | `<!-- toc -->` markers |
| Update Mechanism | Automatic on compile | Script run after markdown generation |
| Anchor Format | Numeric | GitHub-flavored slugs |
| Max Depth | `\setcounter{tocdepth}` | `--max-depth=3` |
| Customization | High | Medium |
| Maintenance | None (automatic) | None (scripted) |

## Customizing TOC Depth

To change the maximum heading level included in the TOC, edit `common/update-toc.sh`:

```bash
# Include only level 2 headings
mdtoc --inplace --max-depth=2 "$file"

# Include up to level 4 headings
mdtoc --inplace --max-depth=4 "$file"
```

## Reference

This approach is based on the Kubernetes enhancements repository:
- https://github.com/kubernetes/enhancements/blob/master/hack/update-toc.sh

## Troubleshooting

### TOC is empty or missing sections

**Check:**
1. Are there `<!-- toc -->` and `<!-- /toc -->` markers?
2. Are there `---` horizontal rules before headings? (Remove them)
3. Are headings using proper Markdown format (`#`, `##`, `###`)?
4. Did the update-toc.sh script run? (Check Makefile)

### Sections appear in wrong order

mdtoc processes headings in document order. Check that your headings are in the correct sequence in the source file.

### Headings have weird formatting in TOC

- Remove HTML tags like `<a name="..."></a>` from headings
- Avoid special characters or formatting in heading text
- Use plain Markdown headings

## Summary

✅ **Automatic generation** - TOC updates every build
✅ **GitHub-compatible** - Uses GitHub-flavored anchors
✅ **No manual maintenance** - Just add/remove/edit headings
✅ **Kubernetes-style** - Proven approach from K8s project
✅ **Simple integration** - One script, two markers, done!
