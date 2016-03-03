MAIN=course-notes
TEX= preface.tex intro/*.tex safety/*.tex ada/*.tex specification/*.tex fault-tolerant-design/*.tex spark/*.tex design-by-contract/*.tex hoare-logic/hoare-logic.tex ttf/ttf.tex
WTEX=$(wildcard $(TEX))
FIGS=intro/figures/*.pdf

all: $(MAIN).pdf

%.pdf: %.tex $(FIGS) $(TEX) ./pagestyle.tex ./packages.tex
	pdflatex $<
	#bibtex $(basename $<)
	sh bibunits
	pdflatex $<
	pdflatex $<

%.ps : %.pdf
	pdftops $<

chapters:
	for d in `ls -d */`; do \
		cd $$d; \
		pwd; \
		make; \
		cd ../; \
	done

clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.out *.bak *.tex~

clean-chapters:
	for d in `ls -d */`; do \
		cd $$d; \
		pwd; \
		make CLEAN; \
		cd ../; \
	done

CLEAN: clean clean-chapters
	rm -f $(MAIN).pdf
