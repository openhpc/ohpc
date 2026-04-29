# Makefile for OpenHPC documentation
#
# Builds all recipes from recipes/*.conf using yq (config merge) + mkdoc.py.
#
# Targets:
#   all        Build all recipes (.md + .sh) [default]
#   script     Build all recipe scripts (.sh)
#   pdf        Build all recipes with PDFs
#   html       Build all recipes as HTML
#   check      Run shellcheck on generated scripts
#   manifest   Regenerate manifest tables from .all files
#   changelog  Generate ChangeLogs (OLD_REF=v4.0.GA)
#   clean      Remove generated files
#   help       Show available targets

PYTHON   ?= python3
MKDOC    := $(PYTHON) mkdoc.py
OLD_REF  ?= v4.0.GA
PANDOC   ?= pandoc
YQ       ?= yq

VC_REVISION := $(shell git log -1 HEAD --pretty=format:%H 2>/dev/null || echo "non-git-checkout")
VC_DATE     := $(shell git log -1 HEAD --pretty=format:%as 2>/dev/null || date -I)

BASE_DIR    := $(CURDIR)
PANDOC_DIR    := $(BASE_DIR)/pandoc
TEMPLATES_DIR := $(BASE_DIR)/templates
FIGURES       := $(TEMPLATES_DIR)/figures

PANDOC_PDF_FLAGS := \
    --pdf-engine=xelatex \
    --number-sections \
    --resource-path $(TEMPLATES_DIR):$(FIGURES):$(BASE_DIR) \
    --lua-filter $(PANDOC_DIR)/format-filters.lua \
    --fail-if-warnings \
    -V geometry:margin=1in \
    -V linkcolor=blue \
    -V urlcolor=blue

PANDOC_HTML_FLAGS := \
    --standalone \
    --embed-resources \
    --toc \
    --number-sections \
    --resource-path $(TEMPLATES_DIR):$(FIGURES):$(BASE_DIR) \
    --lua-filter $(PANDOC_DIR)/format-filters.lua \
    --css $(PANDOC_DIR)/codeblock-styles.css

# Discover recipes by .conf files and derive output names
RECIPES      := $(wildcard recipes/*.conf)
NAMES        := $(basename $(notdir $(RECIPES)))
OUTPUTS_MD   := $(addprefix build/,$(addsuffix .md,$(NAMES)))
OUTPUTS_SH   := $(addprefix build/,$(addsuffix .sh,$(NAMES)))
OUTPUTS_PDF  := $(addprefix build/,$(addsuffix .pdf,$(NAMES)))
OUTPUTS_HTML := $(addprefix build/,$(addsuffix .html,$(NAMES)))

# Coarse-grained dependencies: rebuild .md if any template or manifest changes
CONFIGS       := $(shell find config -name '*.yaml')
TEMPLATES     := $(shell find templates -name '*.md.j2')
MANIFESTS     := $(shell find manifests -name '*.md.j2' -o -name '*.md')
PANDOC_FILES  := $(PANDOC_DIR)/format-filters.lua \
                 $(PANDOC_DIR)/header-includes.tex.j2 \
                 $(PANDOC_DIR)/codeblock-styles.css
DEPS          := mkdoc.py $(TEMPLATES) $(MANIFESTS) $(PANDOC_FILES)

# Manifest directories for manifest/changelog targets
MANIFEST_DIRS := $(wildcard manifests/*/pkg-ohpc.all)
MANIFEST_DIRS := $(dir $(MANIFEST_DIRS))

# shellcheck exclusions (matching POC conventions)
#   SC2154: referenced but not assigned (recipe variables)
#   SC1090: non-constant source
#   SC2129: consider using { cmd1; cmd2; } >> file
SHELLCHECK_EXCLUDE := SC2154,SC1090,SC2129

# Prevent Make from deleting merged config files as intermediates
.PRECIOUS: build/%.yaml

.PHONY: all script pdf html check manifest changelog clean help

.SECONDEXPANSION:

all: $(OUTPUTS_MD) $(OUTPUTS_SH)

script: $(OUTPUTS_SH)

pdf: $(OUTPUTS_PDF)

html: $(OUTPUTS_HTML)

# Pattern rule: merge config YAML files listed in recipes/*.conf with optional
# overrides from recipes/*.yaml (Confluent recipes only; others have no .yaml file).
# .SECONDEXPANSION allows $(wildcard) to expand with the %-stem at rule match time.
build/%.yaml: recipes/%.conf $$(wildcard recipes/$$*.yaml) $(CONFIGS)
	@mkdir -p build
	$(YQ) eval-all '. as $$item ireduce ({}; . * $$item)' \
		$(shell cat recipes/$*.conf) $(wildcard recipes/$*.yaml) | \
	$(YQ) '.vc_revision = "$(VC_REVISION)" | .vc_date = "$(VC_DATE)"' > $@

# Pattern rule: merged config -> .md
build/%.md: build/%.yaml $(DEPS)
	$(MKDOC) $<

# Pattern rule: .md -> .sh (depends on .md being built first)
build/%.sh: build/%.md
	$(MKDOC) build/$*.yaml --with-script

# Pattern rule: .md -> .pdf (header .tex rendered alongside .md by mkdoc.py)
build/%.pdf: build/%.md
	$(PANDOC) $< -o $@ $(PANDOC_PDF_FLAGS) \
		-H build/graphicspath.tex \
		-H build/$*-header.tex

# Pattern rule: .md -> .html
build/%.html: build/%.md
	$(PANDOC) $< -o $@ $(PANDOC_HTML_FLAGS)

check: all
	@if command -v shellcheck >/dev/null 2>&1; then \
		echo "Running shellcheck on build/*.sh..."; \
		shellcheck --exclude=$(SHELLCHECK_EXCLUDE) $(OUTPUTS_SH); \
		echo "shellcheck passed."; \
	else \
		echo "Warning: shellcheck not installed, skipping."; \
	fi

manifest:
	@for d in $(MANIFEST_DIRS); do \
		echo "Generating manifest tables: $$d"; \
		$(PYTHON) generate_manifest.py $$d; \
	done

changelog:
	@for d in $(MANIFEST_DIRS); do \
		echo "Generating changelog: $$d"; \
		$(PYTHON) generate_changelog.py $$d --old-ref $(OLD_REF); \
	done

clean:
	rm -f build/*.md build/*.sh build/*.pdf build/*.html build/*.tex build/*.yaml
	rm -f manifests/*/ChangeLog

help:
	@echo "Targets:"
	@echo "  all        Build all recipes (.md + .sh) [default]"
	@echo "  script     Build all recipe scripts (.sh)"
	@echo "  pdf        Build all recipes with PDFs"
	@echo "  html       Build all recipes as HTML"
	@echo "  check      Run shellcheck on generated scripts"
	@echo "  manifest   Regenerate manifest tables from .all files"
	@echo "  changelog  Generate ChangeLogs (OLD_REF=$(OLD_REF))"
	@echo "  clean      Remove generated files"
	@echo "  help       Show this message"
	@echo ""
	@echo "Individual recipes:"
	@$(foreach n,$(NAMES),echo "  build/$(n).md";)
	@echo ""
	@echo "Individual scripts:"
	@$(foreach n,$(NAMES),echo "  build/$(n).sh";)
	@echo ""
	@echo "Individual PDFs:"
	@$(foreach n,$(NAMES),echo "  build/$(n).pdf";)
	@echo ""
	@echo "Individual HTML:"
	@$(foreach n,$(NAMES),echo "  build/$(n).html";)
