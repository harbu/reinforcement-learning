show: slides.pdf
	chromium slides.pdf

slides.pdf: slides.tex
	pdflatex --shell-escape slides

toc:
	pdflatex --shell-escape slides
	pdflatex --shell-escape slides

bib:
	pdflatex --shell-escape slides
	biber slides
	pdflatex --shell-escape slides
	pdflatex --shell-escape slides

clean:
	rm -f *.aux *.log *.toc *.blg *.bbl *~ *-eps-converted-to.pdf slides.pdf *.vrb *.snm *.run.xml *.out *.nav *.bcf
