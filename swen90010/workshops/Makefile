TUTES=Workshop_Booklet

workshops:
	cd workshop-1/; \
	make; \
	cp workshop_1.pdf ../$(TUTES).pdf; \
	cd ..; \
	for i in `seq 2 9`; do\
		(echo "next $$i"); \
		cd workshop-$$i/; \
		make; \
		cd ..; \
		mv $(TUTES).pdf Cumulative.pdf; \
		pdfjoin --outfile $(TUTES).pdf Cumulative.pdf workshop-$$i/workshop_$$i.pdf; \
		rm -f Cumulative.pdf; \
	done

clean:
	rm -f *.lof *.aux *.log *.toc *.bbl *.blg *.plg *.dvi *.bak *.tex~

CLEAN: clean
	rm -f $(TUTES).pdf
	for i in `seq 1 9`; do\
		(echo "next $$i"); \
		cd workshop-$$i/; \
		make CLEAN; \
		cd ..; \
	done
