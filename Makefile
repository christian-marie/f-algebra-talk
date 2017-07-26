MARKDOWNS=$(wildcard *.md)
DOTS=$(wildcard *.dot)
PDFS=$(MARKDOWNS:md=pdf)
PNGS=$(DOTS:dot=png)
DIAGS=$(wilcard diagrams/*.tex)
DVIS=$(DIAGS:tex=dvi)
EPSS=$(DIAGS:tex=eps)

all: slides.pdf $(PNGS)

clean:
	rm -f $(PDFS) $(PNGS) pp-* images/*

%.pdf: %.md template.tex $(PNGS) $(EPSS)
	cat $< | cats --input - | sed s/\.svg/\.pdf/g > pp-$<
	pandoc -H template.tex --filter columnfilter.py -i -t beamer -s --highlight-style=zenburn pp-$< -o $@

%.png: %.dot
	dot -Tpng $< -o $@
