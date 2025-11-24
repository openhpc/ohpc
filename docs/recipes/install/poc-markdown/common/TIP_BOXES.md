# Tip Boxes - OpenHPC Documentation

## Overview

Tip boxes provide a visually distinct way to highlight important information, optional configurations, or helpful hints in the documentation. They match the original OpenHPC LaTeX styling with a blue border and "Tip" header.

## Syntax

Use a fenced div with the `.tip` class:

```markdown
::: {.tip}
Your tip content goes here. You can include multiple paragraphs,
lists, code blocks, or any other markdown content.
:::
```

## Example

### Markdown Source

```markdown
::: {.tip}
Life is a tale told by an idiot, full of sound and fury signifying nothing. --Willy Shakes
:::
```

### PDF Output

- Blue border (RGB: 96, 170, 210 - OpenHPC logo blue)
- White background
- "Tip" header with blue background and white text
- Clean, professional appearance matching original LaTeX style

### HTML Output

- Blue border (#60aad2)
- White background
- "Tip" header with blue background
- Rounded corners for modern appearance
- Responsive design

## Multi-Paragraph Tips

```markdown
::: {.tip}
This is the first paragraph of the tip.

This is a second paragraph with more details.

You can also include:
- Bullet points
- Code snippets
- **Bold** and *italic* text
:::
```

## With Code Blocks

```markdown
::: {.tip}
To check your OpenHPC installation, run:

```bash
[sms]# dnf repolist | grep OpenHPC
```

This should show the OpenHPC repository is enabled.
:::
```

## Best Practices

1. **Use sparingly** - Tip boxes should highlight important or optional information, not replace regular documentation
2. **Keep concise** - Tips work best when brief and focused
3. **One topic per tip** - Don't combine multiple unrelated tips in one box
4. **Complement, don't duplicate** - Tips should add value, not repeat what's in the main text

## Technical Implementation

### LaTeX (PDF)
- Defined in `common/header-includes.tex`
- Uses `\newenvironment{tipbox}` with `\fcolorbox`
- Lua filter converts `.tip` divs to `\begin{tipbox}...\end{tipbox}`

### HTML
- Defined in `common/format-filters.lua`
- Creates nested divs with inline styles
- Header div with blue background
- Content div with white background and blue border

## Color Scheme

- **Blue border/header**: RGB(96, 170, 210) or #60aad2
- **Background**: White
- **Text color**: Black (content), White (header)

This matches the OpenHPC brand colors from the original documentation.
