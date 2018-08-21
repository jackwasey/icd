# this should work on unices and Windows
HOST := $(shell hostname)
RES3 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 3, Sys.info()["nodename"], sep = "-"), ".R"))')
RES7 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 7, Sys.info()["nodename"], sep = "-"), ".R"))')
NAME := icd-JSS3447-replication
ARTICLE := Efficient_calculation_of_comorbidities_from_medical_codes
README := README-replication
#MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
#PARENT_DIR := $(shell dirname $(MAKEFILE_PATH))

all: quickbench extras

full: fullbench extras

extras: purl readme tar

# replication of benchmark results in icd JSS submission
$(ARTICLE).R:
	Rscript -e 'knitr::purl(system.file("doc", "$(ARTICLE).Rmd", package = "icd"))'

$(README).html:
	Rscript -e 'rmarkdown::render("$(README).Rmd")'

readme: $(README).html

purl: $(ARTICLE).R

deps:
	Rscript install-dependencies.R

$(RES3):
	Rscript bench-versus.R 3

$(RES5):
	Rscript bench-versus.R 5

$(RES7):
	Rscript bench-versus.R 7

quickbench: $(RES3) bench-versus.R

bench: $(RES5)

fullbench: $(RES7)

tar: $(NAME).tar.gz

# COPYFILE_DISABLE=1 to avoid MacOS adding ._ files to tarball
$(NAME).tar.gz:
	$(eval ICDTMP := $(shell mktemp -d))
	@mkdir "$(ICDTMP)/$(NAME)"
	cp * $(ICDTMP)/$(NAME); \
	pushd $(ICDTMP); \
	COPYFILE_DISABLE=1 tar -cvz -f $(NAME).tar.gz --exclude=".*" --exclude=$(NAME).tar.gz --exclude="bench-versus-dput*.R" --exclude=bench-versus-result\*.rds $(NAME); \
	popd; cp $(ICDTMP)/$(NAME).tar.gz .;	rm -rf "$(ICDTMP)"

clean: cleantar
	rm -f $(ARTICLE).R
	rm -f $(README).html

cleantar:
	rm -f $(NAME).tar.gz

cleanbench:
	rm -f bench-versus-result* bench-versus-dput*

.PHONY: bench quickbench fullbench deps all full extras purl tar cleantar cleanbench clean readme
