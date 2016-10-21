PAPER    := hpcsyspros.pdf
TEX_SUFS := .aux .log .nav .out .snm .toc .vrb .fdb_latexmk .bbl .blg .fls


%.pdf:%.tex  $(wildcard *.tex ) hpcsyspros.bib sig-alternate-05-2015.cls acmcopyright.sty
	latexmk -pdf $<

all: $(PAPER)

clean:
	$(RM) $(foreach suf, ${TEX_SUFS}, $(PAPER:.pdf=${suf}))
	$(RM) $(PAPER)

