NUM=2
FIGS=
DIAFIG = figs/*.dia
WCDIAFIG = $(wildcard $(DIAFIG))
DIATOEPSFIG = $(WCDIAFIG:.dia=.eps)
DIATOPDFFIG = $(WCDIAFIG:.dia=.pdf)

DIA=dia
DIAFLAGS=--nosplash
EPSTOPDF=epstopdf

assignment: *.tex $(FIGS) $(DIATOPDFFIG)
	pdflatex Assignment_$(NUM)
	pdflatex Assignment_$(NUM)

solutions: *.tex $(FIGS) $(DIATOPDFFIG)
	pdflatex Assignment_$(NUM)_Solutions
	pdflatex Assignment_$(NUM)_Solutions

all: assignment solutions

%.eps : %.dia
	$(DIA) $(DIAFLAGS) --filter=eps --export=$@ $<

%.pdf : %.eps
	$(EPSTOPDF) $<


clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.dvi *.bak *.tex~

CLEAN: clean
	rm -f Assignment_$(NUM).pdf
	rm -f Assignment_$(NUM)_Solutions.pdf
	rm -f $(DIATOPDFFIG)
