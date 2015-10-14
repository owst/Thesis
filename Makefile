NAME := thesis

.PHONY: $(NAME).pdf

full: $(NAME).pdf

TEX = $(shell find . -name '*.tex')

quick: $(TEX)
	pdflatex $(NAME)

$(NAME).pdf: $(TEX)
	latexmk -pdf -bibtex $(NAME)

preamble:
	latex -ini "&pdflatex preamble.tex\dump"

clean: 
	latexmk -c $(NAME)
	rm -f *.blg *.bbl
