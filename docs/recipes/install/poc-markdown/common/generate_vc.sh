#!/bin/bash
# Generate version control information YAML file

OUTPUT=${1:-vc.yaml}

if git describe >/dev/null 2>&1; then
    VC_REV=$(git log -1 HEAD --pretty=format:"%H")
    VC_DATE=$(git log -1 HEAD --pretty=format:"%as")
else
    VC_REV="non-git-checkout"
    VC_DATE=$(date '+%Y-%m-%d')
fi

cat > "$OUTPUT" <<EOF
# Version control information (auto-generated)
VCRevision: "$VC_REV"
VCDateISO: "$VC_DATE"
EOF
