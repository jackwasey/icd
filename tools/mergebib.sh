#!/bin/bash

#cp vignettes/icd.bib vignettes/all.bib
#cat vignettes/other.bib >> vignettes/all.bib
#cat vignettes/gplv3.bib >> vignettes/all.bib

command -v bibtool >/dev/null 2>&1 || { echo >&2 "I require bibtool but it's not installed. Aborting."; exit 1; }

bibtool -s gplv3.bib other.bib icd.bib > all.bib

