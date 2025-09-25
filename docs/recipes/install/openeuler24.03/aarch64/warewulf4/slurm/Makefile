PAPER    := steps.pdf
TEX_SUFS := .aux .log .nav .out .snm .toc .vrb .fdb_latexmk .bbl .blg .fls
GITID    := $(shell git describe 2> /dev/null)

#$(info $$GITID is [${GITID}])                                                                                                              


%.pdf:%.tex steps.tex manifest.tex $(wildcard common/*.tex common/ohpc-doc.sty common/base_edition/*.tex common/figures/*.pdf manifest/*.tex)
ifdef GITID
	common/vc
else
	test -s vc.tex || { echo "vc.tex file does not exist (and this is not a git repo)!"; exit 1; }
endif
	latexmk -pdf $<

all: $(PAPER)

clean:
	$(RM) $(foreach suf, ${TEX_SUFS}, $(PAPER:.pdf=${suf}))
ifdef GITID
	$(RM) vc.tex
endif
	$(RM) $(PAPER)

