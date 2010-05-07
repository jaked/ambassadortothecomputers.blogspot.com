MLPOST=/home/jake/ocaml-3.11.2/bin/mlpost

FIGS=\
how-froc-works-a.png \
how-froc-works-b.png \
how-froc-works-c.png \
how-froc-works-d.png \
how-froc-works-e.png \
how-froc-works-f.png \

ONES=$(FIGS:.png=.1)

all: $(FIGS)

%.native: %.ml
	$(MLPOST) -native -dont-execute -compile-name $@ $^

$(ONES): fig.native
	./$^ -ps

%.tex: all.template
	sed -e 's/all/$*/' all.template > $@

%.ps: %.1 %.tex
	latex $*
	dvips -E $*.dvi -o

%.png: %.ps
	convert $^ $@

clean:
	rm -f *.tex *.ps *.png *.1 *.dvi *.aux *.log *.native

install:
	cp $(FIGS) ../../img
	cp $(FIGS) ../../_site/img

