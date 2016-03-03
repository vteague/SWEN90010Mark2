MAIN=Sample_Exam
FIGS=
DIAFIG = figs/*.dia
WCDIAFIG = $(wildcard $(DIAFIG))
DIATOPDFFIG = $(WCDIAFIG:.dia=.pdf)

DIA=dia
DIAFLAGS=--nosplash
EPSTOPDF=epstopdf

assignment: *.tex $(FIGS) $(DIATOPDFFIG)
	pdflatex $(MAIN)
	pdflatex $(MAIN)

%.eps : %.dia
	$(DIA) $(DIAFLAGS) --filter=eps --export=$@ $<

%.pdf : %.eps
	$(EPSTOPDF) $<

clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.dvi *.bak *.tex~

CLEAN: clean
	rm -f $(MAIN).pdf
	rm -f $(DIATOPDFFIG)
