TEX=pdflatex

all: manual

manual: flipsman.tex
	$(TEX) $<
	$(TEX) $<
	$(TEX) $<

clean:
	rm -rf flipsman.log
	rm -rf flipsman.aux
	rm -rf flipsman.toc
	rm -rf flipsman.dvi
	rm -rf flipsman.ps
	rm -rf flipsMan.synctex.gz
