# Automatic Source File Markers for Include Statements

## Problem

When building documentation from multiple Jinja2 template files using `{% include %}`, 
it's helpful to know which source file generated each section in the final output. 
This is especially useful for debugging code block width violations and other issues.

## Solution

The `include_with_marker()` macro automatically wraps included content with HTML comment 
markers that identify the source file.

### How It Works

1. **Macro Definition** (`common/macros.j2`):
   ```jinja2
   {% macro include_with_marker(filename) %}
   <!-- SOURCE_FILE: {{ filename }} -->
   {% include filename with context %}
   <!-- END_SOURCE_FILE: {{ filename }} -->
   {% endmacro %}
   ```

2. **Build Process** (Makefile):
   - Concatenates `macros.j2` with the main template file before processing
   - This makes the macro available in the same scope as the template
   - Jinja2 processes the combined file, expanding all macro calls
   - The temporary combined file is automatically cleaned up

3. **Template Usage**:
   ```jinja2
   {{ include_with_marker('common/my-section.md.j2') }}
   ```

4. **Generated Output** (steps.md):
   ```markdown
   <!-- SOURCE_FILE: common/my-section.md.j2 -->
   ...actual content from my-section.md.j2...
   <!-- END_SOURCE_FILE: common/my-section.md.j2 -->
   ```

### Makefile Implementation

```makefile
# Template files
TEMPLATE := steps.md.j2
TEMPLATE_WITH_MACROS := .steps.md.j2.tmp

# Build rule
$(OUTPUT): $(TEMPLATE) $(MERGED_CONFIG) $(wildcard $(COMMON_TEMPLATES)/*.md.j2)
	@# Concatenate macros with template to make macros available in same scope
	@cat $(COMMON_TEMPLATES)/macros.j2 $(TEMPLATE) > $(TEMPLATE_WITH_MACROS)
	$(JINJA2) $(JINJA2_FLAGS) $(TEMPLATE_WITH_MACROS) $(MERGED_CONFIG) -o $(OUTPUT)
	@rm -f $(TEMPLATE_WITH_MACROS)
	$(COMMON_TEMPLATES)/update-toc.sh $(OUTPUT)

# Clean rule
clean:
	rm -f $(OUTPUT) $(VC_YAML) $(MERGED_CONFIG) $(TEMPLATE_WITH_MACROS) steps.pdf
```

### Benefits

1. **Automatic**: No manual marker management required
2. **Recursive**: Works with nested includes - each level gets its own markers
3. **Invisible**: HTML comments are preserved in markdown but stripped by pandoc in PDF
4. **Tool-friendly**: Scripts like `check-codeblock-width.sh` can report exact source files
5. **No scope issues**: Concatenation ensures macros have access to template variables

### Example Output

With 67 direct includes in the main template, plus nested includes in common files, 
the generated `steps.md` contains 184 SOURCE_FILE markers (92 start/end pairs), 
providing complete traceability for every included section.

### Migration from Plain Includes

To convert existing templates:

**Before:**
```jinja2
{% include 'common/section.md.j2' %}
```

**After:**
```jinja2
{{ include_with_marker('common/section.md.j2') }}
```

No import statement needed - the macro is automatically available through concatenation.
