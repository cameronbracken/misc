R-seminar.pdf: R-seminar.tex
	pdflatex R-seminar.tex
R-seminar.tex: R-seminar.Rnw
	R CMD Sweave R-seminar.Rnw
clean:
	rm *.{pdf,tex,aux,log} figs/*
