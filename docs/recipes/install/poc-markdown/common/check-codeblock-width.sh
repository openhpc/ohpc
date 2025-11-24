#!/bin/bash
# Script to check that no code block lines exceed 77 characters
# This ensures lines fit on the PDF page without wrapping
#
# Environment variables:
#   OHPC_CODEBLOCK_WIDTH_CHECK=warn  - Convert errors to warnings (allow PDF generation)
#   OHPC_CODEBLOCK_WIDTH_CHECK=error - Treat as errors (default, blocks PDF generation)
#   OHPC_CODEBLOCK_WIDTH_CHECK=off   - Skip check entirely

set -o pipefail

MAX_WIDTH=86
EXIT_CODE=0
CHECK_MODE="${OHPC_CODEBLOCK_WIDTH_CHECK:-error}"

# Color codes for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

if [ $# -lt 1 ]; then
    echo "Usage: $0 <markdown-file>" >&2
    echo "Environment: OHPC_CODEBLOCK_WIDTH_CHECK=[error|warn|off] (default: error)" >&2
    exit 1
fi

# Handle check mode
if [ "$CHECK_MODE" = "off" ]; then
    echo -e "${BLUE}Code block width checking disabled (OHPC_CODEBLOCK_WIDTH_CHECK=off)${NC}"
    exit 0
fi

MARKDOWN_FILE="$1"

if [ ! -f "$MARKDOWN_FILE" ]; then
    echo -e "${RED}Error: File not found: $MARKDOWN_FILE${NC}" >&2
    exit 1
fi

# Temporary file to store the current code block
TEMP_BLOCK=$(mktemp)
trap "rm -f $TEMP_BLOCK" EXIT

# Track state
IN_CODE_BLOCK=false
CODE_BLOCK_START_LINE=0
CURRENT_LINE=0
CURRENT_SOURCE_FILE="(main template)"
ERRORS_FOUND=0

# Read the markdown file line by line
while IFS= read -r line || [ -n "$line" ]; do
    CURRENT_LINE=$((CURRENT_LINE + 1))

    # Track source file from HTML comments (if present)
    if [[ "$line" =~ \<!--[[:space:]]*SOURCE_FILE:[[:space:]]*([^[:space:]]+)[[:space:]]*--\> ]]; then
        CURRENT_SOURCE_FILE="${BASH_REMATCH[1]}"
    elif [[ "$line" =~ \<!--[[:space:]]*END_SOURCE_FILE:[[:space:]]*([^[:space:]]+)[[:space:]]*--\> ]]; then
        CURRENT_SOURCE_FILE="(main template)"
    fi

    # Check for code block delimiter (```)
    if [[ "$line" =~ ^\`\`\` ]]; then
        if [ "$IN_CODE_BLOCK" = false ]; then
            # Starting a new code block
            IN_CODE_BLOCK=true
            CODE_BLOCK_START_LINE=$CURRENT_LINE
            > "$TEMP_BLOCK"
        else
            # Ending a code block
            IN_CODE_BLOCK=false

            # Check all lines in this code block
            BLOCK_LINE=0
            while IFS= read -r code_line || [ -n "$code_line" ]; do
                BLOCK_LINE=$((BLOCK_LINE + 1))
                LINE_LENGTH=${#code_line}

                if [ $LINE_LENGTH -gt $MAX_WIDTH ]; then
                    if [ $ERRORS_FOUND -eq 0 ]; then
                        if [ "$CHECK_MODE" = "warn" ]; then
                            echo -e "\n${YELLOW}WARNING: Code block lines exceeding $MAX_WIDTH characters found!${NC}\n"
                            echo "Lines longer than $MAX_WIDTH characters may not fit on the PDF page and could be truncated or wrapped,"
                            echo "making the document difficult to read and commands harder to copy-paste correctly."
                            echo -e "${YELLOW}PDF generation will continue (OHPC_CODEBLOCK_WIDTH_CHECK=warn)${NC}"
                        else
                            echo -e "\n${RED}ERROR: Code block lines exceeding $MAX_WIDTH characters found!${NC}\n"
                            echo "Lines longer than $MAX_WIDTH characters will not fit on the PDF page and will be truncated or wrapped,"
                            echo "making the document difficult to read and commands impossible to copy-paste correctly."
                        fi
                        echo ""
                    fi

                    ERRORS_FOUND=$((ERRORS_FOUND + 1))
                    ACTUAL_LINE=$((CODE_BLOCK_START_LINE + BLOCK_LINE))

                    echo -e "${BLUE}Source file: ${CURRENT_SOURCE_FILE}${NC}"
                    echo -e "${YELLOW}Generated file: $MARKDOWN_FILE, line $ACTUAL_LINE${NC}"
                    echo -e "${YELLOW}Code block starts at line: $CODE_BLOCK_START_LINE${NC}"
                    echo -e "${YELLOW}Line length: $LINE_LENGTH characters (exceeds limit by $((LINE_LENGTH - MAX_WIDTH)))${NC}"
                    echo -e "${RED}Content:${NC} ${code_line:0:100}$([ $LINE_LENGTH -gt 100 ] && echo '...')"
                    echo ""
                    echo -e "${GREEN}Solution:${NC} Break this command using a backslash (\\) continuation:"
                    echo "  Example:"
                    echo "    [sms]# very-long-command --with --many --options \\"
                    echo "             --more-options --even-more"
                    echo ""
                    echo "---"
                    echo ""
                fi
            done < "$TEMP_BLOCK"
        fi
    elif [ "$IN_CODE_BLOCK" = true ]; then
        # Store the line in the temporary block file
        echo "$line" >> "$TEMP_BLOCK"
    fi
done < "$MARKDOWN_FILE"

if [ $ERRORS_FOUND -eq 0 ]; then
    echo -e "${GREEN}✓ All code block lines in $MARKDOWN_FILE are within $MAX_WIDTH characters${NC}"
    exit 0
else
    if [ "$CHECK_MODE" = "warn" ]; then
        echo -e "${YELLOW}Found $ERRORS_FOUND line(s) exceeding $MAX_WIDTH characters in code blocks.${NC}"
        echo -e "${YELLOW}Consider fixing these issues in the source .md.j2 files for better PDF readability.${NC}"
        echo -e "${YELLOW}PDF generation will continue (use OHPC_CODEBLOCK_WIDTH_CHECK=error to block on errors)${NC}"
        exit 0  # Exit successfully to allow PDF generation
    else
        echo -e "${RED}Found $ERRORS_FOUND line(s) exceeding $MAX_WIDTH characters in code blocks.${NC}"
        echo -e "${RED}Please fix these issues in the source .md.j2 files before generating the PDF.${NC}"
        echo -e "${BLUE}Tip: Use OHPC_CODEBLOCK_WIDTH_CHECK=warn to generate PDF anyway (not recommended)${NC}"
        exit 1  # Exit with error to block PDF generation
    fi
fi
