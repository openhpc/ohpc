#!/bin/bash
# OpenHPC Installation Recipe
# Auto-generated from markdown documentation

set -e

# --- +section: chapters/example.md.j2 ---
# --- +section: example/head.md.j2 ---
echo '=== section: example/head.md.j2 ==='
echo '--- Install the base packages (it'\''s quoted)'
dnf -y install ohpc-base

if [[ "${enable_feature}" -eq 1 ]];then
dnf -y install feature-ohpc
else
echo "feature disabled"
fi
if [[ "${num_computes}" -gt 0 && -e "${inputFile}" ]];then
systemctl status ohpc.service
fi
#<<< ohpc_proxy:head >>>#
# --- -section: example/head.md.j2 ---
# --- +section: example/compute.md.j2 ---
echo '=== section: example/compute.md.j2 ==='
wwctl image build rocky-10
# --- -section: example/compute.md.j2 ---
# --- -section: chapters/example.md.j2 ---
