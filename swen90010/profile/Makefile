TARGET=profile
FIG=
REFERENCES=
WORDCOUNT=
INCLUDES=profile.tex

all: $(TARGET).pdf

$(TARGET).pdf: $(TARGET).tex  $(INCLUDES) $(BIBLIOGRAPHY) $(REFERENCES) $(DIATOPDFFIG)
	pdflatex $(TARGET)


clean:
	rm -f *.lof *.aux *.log *.toc *.lot *.bbl *.blg *.plg *.dvi *.bak *.tex~

CLEAN: clean
	rm -f $(TARGET).pdf
