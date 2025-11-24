# PDF Generation Guide

## Overview

The Markdown documentation can be converted to PDF using Pandoc. Page breaks are controlled using LaTeX directives in the Markdown. Table of contents is automatically generated using `mdtoc` (see [TOC_GENERATION.md](TOC_GENERATION.md)).

The title page uses HTML `<p>` tags with inline styling instead of markdown headings to avoid chapter numbering when `--number-sections` is enabled.

## Table of Contents

The current PDF generation does **not** automatically generate a table of contents. If you want a ToC in the PDF, you have two options:

### Option 1: Manual ToC in Markdown

Add a table of contents section directly in your `steps.md.j2`:

```markdown
## Table of Contents

1. [Introduction](#introduction)
2. [Install Base Operating System](#install-bos)
3. [Install OpenHPC Components](#install-components)
   - [Enable OpenHPC repository](#enable-repo)
   - [Add resource management services](#add-rm)
```

### Option 2: Enable Pandoc's Automatic ToC

Uncomment the `--toc` flag in the Makefile:

```makefile
pdf: $(OUTPUT)
	pandoc $(OUTPUT) -o steps.pdf \
		--pdf-engine=xelatex \
		--toc \                      # Uncomment to enable automatic ToC
		--toc-depth=3 \              # Control ToC depth
		--number-sections \
		-V geometry:margin=1in \
		-V linkcolor:blue \
		-V urlcolor:blue
```

This will automatically:
1. Parse all headings (##, ###, etc.) in the Markdown
2. Generate a "Contents" section at the beginning
3. Add clickable links to each section
4. Number sections based on `--number-sections` flag

## Title Page Formatting

The title page uses HTML `<p>` tags with inline CSS styling instead of markdown headings:

```markdown
<div align="right">
<img src="../../../../common/figures/ohpc_logo_blue.png" width="4cm">
</div>

<div align="center">
<p style="font-size: 32px; font-weight: bold; font-family: sans-serif;">OpenHPC (v4.0)</p>
<p style="font-size: 24px; font-weight: bold; font-family: sans-serif;">Cluster Building Recipes</p>
```

**Why not use markdown headings?**

When Pandoc's `--number-sections` flag is enabled, it adds chapter numbers to ALL headings, including those on the title page:

```markdown
# OpenHPC (v4.0)           ← Would become "1 OpenHPC (v4.0)" in PDF
## Cluster Building Recipes  ← Would become "1.1 Cluster Building Recipes"
```

By using styled HTML paragraphs, the title page has large, bold text without chapter numbers.

**Styling:**
- **Font family**: Sans-serif for clean, modern appearance
- **Main title**: 32px, bold
- **Subtitle**: 24px, bold
- **OS/Edition info**: 20px, bold
- **Logo**: Right-aligned, 4cm width, no caption

These can be adjusted in `common/title.md.j2` by changing the inline CSS styles.

## Page Breaks

### After Title Page

The title template includes `\newpage` at the end:

```markdown
</div>

\newpage
```

This creates a page break between the title page and the table of contents.

### Between Sections

To add page breaks elsewhere in the document, use:

```markdown
\newpage

## Next Section
```

Or for a more Markdown-native approach, use:

```markdown
<div style="page-break-after: always;"></div>

## Next Section
```

## Pandoc Options Explained

From the Makefile:

```makefile
pdf: $(OUTPUT)
	pandoc $(OUTPUT) -o steps.pdf \
		--pdf-engine=xelatex \      # Use XeLaTeX for better Unicode/font support
		--number-sections \          # Number all sections (1, 1.1, 1.1.1, etc.)
		-V geometry:margin=1in \     # Set page margins
		-V linkcolor:blue \          # Color for internal links
		-V urlcolor:blue             # Color for external URLs
```

## Customizing the Table of Contents

### Change ToC Depth

Only show top 2 levels:
```makefile
--toc-depth=2
```

### Disable Section Numbering

Remove the `--number-sections` flag.

### Custom ToC Title

Add to Makefile:
```makefile
-V toc-title:"Table of Contents"
```

## Image Handling

Images in the title page use relative paths:

```markdown
![OpenHPC Logo](../../../../common/figures/ohpc_logo_blue.png)
```

Pandoc resolves these paths relative to the Markdown file location.

## Common Issues and Solutions

### Issue: Want Automatic Table of Contents

**Problem**: Want Pandoc to automatically generate table of contents.

**Solution**: Add the `--toc` flag to the Makefile:

```makefile
pdf: $(OUTPUT)
	pandoc $(OUTPUT) -o steps.pdf \
		--pdf-engine=xelatex \
		--toc \                    # Add this line
		--toc-depth=3 \            # Add this line
		--number-sections \
		-V geometry:margin=1in \
		-V linkcolor:blue \
		-V urlcolor:blue
```

### Issue: Logo Not Found in PDF

**Problem**: Image path doesn't resolve correctly.

**Solution**: Use relative paths from the Markdown file location:
```markdown
![Logo](../../../../common/figures/ohpc_logo_blue.png)
```

### Issue: No Page Break After Title

**Problem**: Title and ToC run together.

**Solution**: Add `\newpage` at the end of `title.md.j2`:
```markdown
</div>

\newpage
```

### Issue: ToC Not Appearing

**Problem**: PDF has no table of contents.

**Solution**: Ensure `--toc` flag is in the pandoc command:
```makefile
pandoc $(OUTPUT) -o steps.pdf --toc
```

## Dynamic ToC Generation (Optional)

If you enable the `--toc` flag, the table of contents is **fully dynamic** and automatically generated by Pandoc:

1. **Scans all headings**: Pandoc finds all `##`, `###`, etc. in the Markdown
2. **Builds hierarchy**: Creates nested structure based on heading levels
3. **Generates links**: Makes each ToC entry clickable
4. **Updates automatically**: Any changes to headings are reflected in ToC

**No manual maintenance required!**

To enable, add `--toc` to the pandoc command in the Makefile (see "Customizing the Table of Contents" above).

## Advanced Options

### Custom Fonts

```makefile
-V mainfont="Times New Roman" \
-V monofont="Courier New"
```

### Custom Colors

```makefile
-V linkcolor:darkblue \
-V urlcolor:purple \
-V toccolor:black
```

### Paper Size

```makefile
-V papersize:a4        # or letter (default)
```

### Syntax Highlighting

```makefile
--highlight-style=tango   # or kate, pygments, espresso, etc.
```

## Example: Full Custom PDF Command

```bash
pandoc steps.md -o steps.pdf \
  --pdf-engine=xelatex \
  --toc \
  --toc-depth=3 \
  --number-sections \
  -V geometry:margin=1in \
  -V linkcolor:darkblue \
  -V urlcolor:blue \
  -V mainfont="Liberation Sans" \
  -V monofont="Liberation Mono" \
  -V papersize:letter \
  --highlight-style=tango \
  -V toc-title:"Table of Contents"
```

## Testing

After generating PDF:

```bash
# Check file was created
ls -lh steps.pdf

# View with a PDF reader (if GUI available)
evince steps.pdf

# Or extract text to verify content
pdftotext steps.pdf - | head -50
```

## Comparison with LaTeX System

| Feature | LaTeX | Markdown+Pandoc |
|---------|-------|-----------------|
| ToC Generation | `\tableofcontents` | Manual or `--toc` flag (optional) |
| Page Breaks | `\newpage` | `\newpage` (same!) |
| Section Numbering | Automatic | `--number-sections` |
| Cross-references | `\ref{}` | Pandoc filter needed |
| Customization | High (LaTeX commands) | Medium (Pandoc options) |
| Syntax Complexity | High | Low |

## Summary

✅ **Page breaks work** - Use `\newpage` in Markdown
✅ **ToC is optional** - Add manually or enable `--toc` flag for automatic generation
✅ **Images resolve** - Use relative paths
✅ **Section numbering** - Automatic with `--number-sections`
✅ **Highly customizable** - Many Pandoc options available
