#!/usr/bin/env python3
"""
Preprocessor that adds SOURCE_FILE markers around {% include %} statements.
This allows tracking which source file generated each section in the output.
"""

import re
import sys

def add_source_markers(content):
    """Add HTML comment markers around {% include %} statements"""
    
    # Pattern to match {% include 'filename' %} or {% include "filename" %}
    include_pattern = r'{%\s*include\s+["\']([^"\']+)["\']\s*%}'
    
    def replace_include(match):
        filename = match.group(1)
        return (
            f'<!-- SOURCE_FILE: {filename} -->\n'
            f'{match.group(0)}\n'
            f'<!-- END_SOURCE_FILE: {filename} -->'
        )
    
    # Replace all include statements with marked versions
    result = re.sub(include_pattern, replace_include, content)
    
    return result

def main():
    if len(sys.argv) != 3:
        print("Usage: add-source-markers.py <input_file> <output_file>", file=sys.stderr)
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    # Read input file
    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Add markers
    processed = add_source_markers(content)
    
    # Write output file
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(processed)

if __name__ == '__main__':
    main()
