# this should work on unices and Windows
HOST := $(shell hostname)
RES3 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 3, Sys.info()["nodename"], sep = "-"), ".R"))')
RES5 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 5, Sys.info()["nodename"], sep = "-"), ".R"))')
RES7 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 7, Sys.info()["nodename"], sep = "-"), ".R"))')
NAME := icd-JSS3447-replication
ARTICLE := Efficient_calculation_of_comorbidities_from_medical_codes
README := README-replication
#MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
#PARENT_DIR := $(shell dirname $(MAKEFILE_PATH))

replicate: bench

all: quickbench extras

full: fullbench extras

extras: purl readme tar

# replication of benchmark results in icd JSS submission
$(ARTICLE).R:
	@echo 'icd' must be installed for the extraction of replication code,
	@echo or 'knitr::purl' should be called directly from the source tree.
	Rscript -e 'knitr::purl(system.file("doc", "$(ARTICLE).Rmd", package = "icd"), output = "$(ARTICLE).R")'

$(README).html:
	Rscript -e 'rmarkdown::render("$(README).Rmd")'

readme: $(README).html

purl: $(ARTICLE).R

deps: lib/icd.data/DESCRIPTION

lib/icd.data/DESCRIPTION:
	Rscript install-dependencies.R

$(RES3): deps
	Rscript bench-versus.R 3

$(RES5): deps
	Rscript bench-versus.R 5

$(RES7): deps
	Rscript bench-versus.R 7

quickbench: $(RES3) bench-versus.R

bench: $(RES5) bench-versus.R

fullbench: $(RES7) bench-versus.R

tar: $(NAME).tar.gz

# COPYFILE_DISABLE=1 to avoid MacOS adding ._ files to tarball
$(NAME).tar.gz:
	$(eval ICDTMP := $(shell mktemp -d))
	@mkdir "$(ICDTMP)/$(NAME)"
	cp * $(ICDTMP)/$(NAME); \
	olddir="$$PWD"; \
	cd $(ICDTMP); \
	COPYFILE_DISABLE=1 tar -cvz -f $(NAME).tar.gz --exclude=".*" --exclude=$(NAME).tar.gz --exclude="bench-versus-dput*.R" --exclude=bench-versus-result\*.rds $(NAME); \
	cd "$$olddir";	cp $(ICDTMP)/$(NAME).tar.gz "$$olddir"; rm -rf "$(ICDTMP)"

clean: cleantar
	rm -f $(ARTICLE).R
	rm -f $(README).html

cleantar:
	rm -f $(NAME).tar.gz

cleanbench:
	rm -f bench-versus-result* bench-versus-dput*

dockerbuild:
	docker build -t icd-jss3447-replication .

dockerrun:
	docker run --rm -ti icd-jss3447-replication

dockerbash:
	docker run --rm -ti icd-jss3447-replication bash

.PHONY: bench quickbench fullbench deps all full extras purl tar cleantar cleanbench clean readme replicate dockerbuild dockerrun dockerbash
