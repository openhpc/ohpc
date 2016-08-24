PAPER    := hpcsyspros.pdf
TEX_SUFS := .aux .log .nav .out .snm .toc .vrb .fdb_latexmk .bbl .blg .fls


%.pdf:%.tex  $(wildcard *.tex ) hpcsyspros.bib
	latexmk -pdf $<

all: $(PAPER)

clean:
	$(RM) $(foreach suf, ${TEX_SUFS}, $(PAPER:.pdf=${suf}))
	$(RM) $(PAPER)

