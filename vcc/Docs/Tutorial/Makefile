-include Makefile.local

D = tutorial

CONV ?= ../../Tools/VccSyntaxCoverter/bin/Debug/VccSyntaxCoverter.exe

all: build 
#maybe-show

build: conv
	pdflatex $(D).tex </dev/null
	chmod +x $(D).pdf
	rm c/*.c.out

.PHONY: conv

conv:
	$(CONV) c/*.c

bib:
	bibtex $(D)
a:
	$(MAKE) build
	$(MAKE) bib
	$(MAKE) build
	$(MAKE) build

show: show-win

maybe-show:
	case `uname` in \
	  *CYGWIN* ) $(MAKE) dont-show-win ;; \
	  *Darwin* ) $(MAKE) show-mac ;; \
	  *Linux* ) $(MAKE) show-lin ;; \
	  * ) ;; \
	esac

show-mac:
	open -a Preview $(D).pdf

dont-show-win:
	true

show-win:
	chmod +x $(D).pdf
	cmd /c $(D).pdf &

show-lin:
	acroread $(D).pdf &

show-ev:
	evince $(D).pdf

show-ac:
	n=$(D)-`md5sum $(D).pdf | sed -e 's/ .*//'`.pdf; \
	cp $(D).pdf $$n; \
	acroread $$n; \
	sleep 2; rm $$n

%.pdf: %.dia
	dia -e `basename $@ .pdf`.eps -t eps $<
	epstopdf `basename $@ .pdf`.eps

clean:
	rm -f *.log *.dvi *.ps *.aux *.pdf *.bbl *.blg *.out

