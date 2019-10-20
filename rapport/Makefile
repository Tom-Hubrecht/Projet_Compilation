report=Rapport

$(report).pdf: $(report).tex
	pdflatex $(report)
	pdflatex $(report)

clean:
	rm -f *.aux *.log *.toc *.out *.pdf *.synctex.gz

.PHONY: clean

