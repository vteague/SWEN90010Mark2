TEX=chapter.tex
WTEX=$(wildcard $(TEX))
TARGETS=$(WTEX:.tex=.pdf)
FIGS=figures/*.*

all: $(TARGETS)

%.pdf: %.tex $(FIGS) *.tex ../pagestyle.tex ../packages.tex
	pdflatex $<
	bibtex $(basename $<)
	pdflatex $<
	pdflatex $<

%.ps : %.pdf
	pdftops $<

clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.out *.bak *.tex~

CLEAN: clean
	rm -f $(TARGETS)
