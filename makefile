N=article

$N.pdf: $N.tex
	pdflatex $N
	pdflatex $N

clean:
	rm -rf *.toc *.log *.aux
