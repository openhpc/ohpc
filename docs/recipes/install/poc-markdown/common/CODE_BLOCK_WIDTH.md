# Code Block Line Width Validation

## Problem

PDF generation requires that all code block lines fit within **77 characters** to avoid:
- Text wrapping that breaks commands
- Content extending beyond page margins
- Difficulty copying and pasting commands

## Solution

The build system includes automatic validation via `check-codeblock-width.sh` that runs before PDF generation.

### Automatic Checking

When you run `make pdf`, the checker script will:
1. Scan all code blocks in the generated markdown
2. Identify lines exceeding 77 characters
3. Report each violation with:
   - Source file (if markers are present)
   - Line number in generated file
   - Line length and excess characters
   - Actual content
   - Suggested fix

### Example Output

```
ERROR: Code block lines exceeding 77 characters found!

Source file: common/enable_ohpc_repo.md.j2
Generated file: steps.md, line 331
Code block starts at line: 330
Line length: 105 characters (exceeds limit by 28)
Content: [sms]# dnf install http://repos.openhpc.community/OpenHPC/4/EL_10/x86_64/ohpc-release-4-1.el10.x86_6...

Solution: Break this command using a backslash (\) continuation:
  Example:
    [sms]# very-long-command --with --many --options \
             --more-options --even-more
```

### Fixing Long Lines

**Option 1: Use backslash continuation**
```bash
# Before (105 characters)
[sms]# dnf install http://repos.openhpc.community/OpenHPC/4/EL_10/x86_64/ohpc-release-4-1.el10.x86_64.rpm

# After (split into two lines)
[sms]# dnf install \
    http://repos.openhpc.community/OpenHPC/4/EL_10/x86_64/ohpc-release-4-1.el10.x86_64.rpm
```

**Option 2: Split into multiple commands**
```bash
# Before (93 characters)
[sms]# ipmitool -E -I lanplus -H ${bmc_ipaddr} -U root chassis bootdev pxe options=persistent

# After (using variables or splitting)
[sms]# export BMC_OPTS="-E -I lanplus -H ${bmc_ipaddr} -U root"
[sms]# ipmitool $BMC_OPTS chassis bootdev pxe options=persistent
```

**Option 3: Use shorter variable names or paths**
```bash
# If the command is inherently too long, consider:
# - Using shorter option names
# - Breaking complex operations into steps
# - Using environment variables for long paths
```

## Adding Source File Markers (Optional)

To track which source `.md.j2` file generated each section, you can add HTML comment markers:

### Using the Macro

In your main template file, import the macros:
```jinja2
{% from 'common/macros.j2' import include_with_marker %}
```

Then use the macro instead of plain includes:
```jinja2
{# Before #}
{% include 'common/myfile.md.j2' %}

{# After #}
{{ include_with_marker('common/myfile.md.j2') }}
```

This adds invisible HTML comments that the checker uses to report the source file.

### Manual Markers

You can also manually add markers in the main template:
```jinja2
<!-- SOURCE_FILE: common/myfile.md.j2 -->
{% include 'common/myfile.md.j2' %}
<!-- END_SOURCE_FILE: common/myfile.md.j2 -->
```

**Note**: HTML comments are preserved in markdown but stripped by pandoc during PDF generation, so they don't appear in the final output.

## Controlling Check Behavior

The validation behavior can be controlled via the `OHPC_CODEBLOCK_WIDTH_CHECK` environment variable:

### Error Mode (Default)
```bash
# Blocks PDF generation if violations found
make pdf
# or explicitly
OHPC_CODEBLOCK_WIDTH_CHECK=error make pdf
```

### Warning Mode
```bash
# Shows warnings but allows PDF generation to continue
OHPC_CODEBLOCK_WIDTH_CHECK=warn make pdf
```

**Use case**: Generate a draft PDF while working on fixing violations.

**Warning**: The generated PDF may have formatting issues with wrapped or truncated lines.

### Disabled Mode
```bash
# Skip the check entirely
OHPC_CODEBLOCK_WIDTH_CHECK=off make pdf
```

**Use case**: Emergency PDF generation when the check itself has issues.

**Warning**: Not recommended for production builds.

## Running the Checker Manually

```bash
# Check with default behavior (error mode)
./common/check-codeblock-width.sh steps.md

# Check with warning mode
OHPC_CODEBLOCK_WIDTH_CHECK=warn ./common/check-codeblock-width.sh steps.md

# Skip the check
OHPC_CODEBLOCK_WIDTH_CHECK=off ./common/check-codeblock-width.sh steps.md
```

## Integration in Makefiles

The checker is automatically integrated in the PDF target:

```makefile
pdf: $(OUTPUT)
	@echo "Checking code block line widths..."
	@$(COMMON_TEMPLATES)/check-codeblock-width.sh $(OUTPUT)
	pandoc $(OUTPUT) -o steps.pdf ...
```

If violations are found, the build stops before running pandoc.

## Character Count Guidelines

- **Maximum**: 77 characters per line in code blocks
- **Recommended**: Keep lines under 75 characters for safety margin
- **Comments**: Long comments in code blocks also count!

## Tips

1. **Test early**: Run `make pdf` frequently to catch issues early
2. **Format commands**: Use `\` continuation for long commands
3. **Variable substitution**: Remember that Jinja2 variables expand, so `{{ very_long_variable_name }}` counts as its full expanded length
4. **Review output**: Check `steps.md` to see actual expanded line lengths
