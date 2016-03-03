FIGS=

all: Assignment_[1-9].pdf

%.pdf : %.tex $(FIGS)
	pdflatex $<
	pdflatex $<

clean:
	rm -f *.lof *.aux *.log *.out *.toc *.bbl *.blg *.plg *.dvi *.bak *.tex~

CLEAN: clean
	rm -f Assignment_[1-9].pdf
