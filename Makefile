SRCDIR=..

notes += intro linearity likelihood optimization challenges

nnhtml := $(notes:%=notes/%.html)
nnrmd := $(notes:%=notes/%.rmd)
## nnslides := notes/glmm.slides.html #$(notes:%=notes/%.slides.html)

Datasets +=  algae.csv ## aids.csv  Banta.RData  gopherdat2.csv culcitalogreg.csv gopherdat2.RData starling.RData culcita.RData gophertortoise.txt toenail.csv dufemalepers.csv tundra.csv Elston2001_tickdata.txt lizards.csv tundra_agg.rda

## Rfiles += geom_cstar.R calcDenDF.R allFit.R montag.R

dd := $(Datasets:%=data/%)
rr := $(Rfiles:%=R/%)


## syllabus.html 
all: nonlin_data.zip setup.html datasets.html ${nnhtml} ${nnrmd} ${nnslides} ${rr}

notes/%.rmd:  ${SRCDIR}/notes/%.[Rr]md
	cp $< $@

R/%.R:  ${SRCDIR}/R/%.R
	cp $< $@

%.html: ${SRCDIR}/%.[Rr]md
	echo "rmarkdown::render(\"$<\",output_dir='.')" | R --slave

%.pdf: ${SRCDIR}/%.rmd
	echo "rmarkdown::render(\"$<\",output_dir='.',output_format=\"pdf_document\")" | R --slave

notes/challenges.html: ${SRCDIR}/notes/challenges.rmd
	R CMD BATCH ${SRCDIR}/notes/vonesh_slice.R
	echo "rmarkdown::render(\"$<\",output_format='html_document',output_dir='notes')" | R --slave

notes/%.html: ${SRCDIR}/notes/%.rmd
	echo "rmarkdown::render(\"$<\",output_format='html_document',output_dir='notes')" | R --slave

notes/%.slides.html: ${SRCDIR}/notes/%.rmd
	echo "rmarkdown::render(\"$<\",,output_file=\"$@\",output_format='ioslides_presentation',output_dir='notes')" | R --slave

notes/%.pdf: ${SRCDIR}/notes/%.rmd
	echo "rmarkdown::render(\"$<\",output_format='tufte_handout',output_dir='notes')" | R --slave

nonlin_data.zip: 
	cd ..; zip gh-pages/nonlin_data.zip ${dd}

datasets.html: ../datasets.csv ../datasets.rmd
