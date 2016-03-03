TEX=workshop_*.tex
WTEX=$(wildcard $(TEX))
TARGETS=$(WTEX:.tex=.pdf)
FIG=figs/*
REFERENCES=
WORDCOUNT=
INCLUDES=*.tex 
BASEFIG=$(subst .fig,,$(FIG))
EPS=$(subst .fig,.eps,$(FIG))

all: $(TARGETS)

%.pdf: %.tex $(FIGS)
	pdflatex $<
	pdflatex $<

%.ps : %.pdf
	pdftops $<

code.zip:
	zip code.zip code/arith.* code/conditionalswap.* code/default.gpr code/linear_search.* code/sum.* code/task4.* code/swap.*

clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.out *.bak *.tex~

CLEAN: clean
	rm -f $(TARGETS)
