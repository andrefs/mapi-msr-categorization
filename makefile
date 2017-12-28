N=article

$N.pdf: *.tex *.bib
	pdflatex $N
	bibtex $N
	pdflatex $N

check:
	aspell -l en -c -t *.tex

clean:
	rm -f *.aux *.toc *. *.bbl *.blg *.pdf *.log *.nav *.out *.snm *.vrb
