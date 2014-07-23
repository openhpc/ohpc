PAPER    := steps.pdf
TEX_SUFS := .aux .log .nav .out .snm .toc .vrb .fdb_latexmk .bbl .blg .fls

%.pdf:%.tex steps.tex ../../common/inputs.tex
	../../common/vc
	latexmk -pdf $<

all: $(PAPER)

clean:
	$(RM) $(foreach suf, ${TEX_SUFS}, $(PAPER:.pdf=${suf})) vc.tex
	$(RM) $(PAPER)



