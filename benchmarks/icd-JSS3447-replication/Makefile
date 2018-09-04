R_HOME := "$(shell R RHOME)"
ifeq ($(OS),Windows_NT)
	RSCRIPT := $(shell echo '$(R_HOME)\\bin\\Rscript')
else
	RSCRIPT := $(shell echo '$(R_HOME)/bin/Rscript')
endif
HOST := $(shell hostname)
RES3 := dput-latest-n-3.R
RES4 := dput-latest-n-4.R
RES5 := dput-latest-n-5.R
RES6 := dput-latest-n-6.R
RES7 := dput-latest-n-7.R
RES8 := dput-latest-n-8.R
NAME := icd-JSS3447-replication
ARTICLE := Efficient_calculation_of_comorbidities_from_medical_codes
README := README-replication

default: result3

all: result3 purl readme tar

# replication of benchmark results in icd JSS submission
$(ARTICLE).R:
	$(RSCRIPT) -e 'knitr::purl(system.file("doc", "$(ARTICLE).Rmd", package = "icd"), output = "$(ARTICLE).R")'

$(README).html:
	$(RSCRIPT) -e 'rmarkdown::render("$(README).Rmd")'

readme: $(README).html

purl: $(ARTICLE).R

deps: .deps

.deps:
	$(RSCRIPT) install-dependencies.R

$(RES3): .deps
	$(RSCRIPT) bench-versus.R 3

$(RES4): .deps
	$(RSCRIPT) bench-versus.R 4

$(RES5): .deps
	$(RSCRIPT) bench-versus.R 5

$(RES6): .deps
	$(RSCRIPT) bench-versus.R 6

$(RES7): .deps
	$(RSCRIPT) bench-versus.R 7

$(RES8): .deps
	$(RSCRIPT) bench-versus.R 8

bench3: $(RES3) bench-versus.R

bench4: $(RES4) bench-versus.R

bench5: $(RES5) bench-versus.R

bench6: $(RES6) bench-versus.R

bench7: $(RES7) bench-versus.R

bench8: $(RES8) bench-versus.R

result3: $(RES3)
	Rscript -e 'dget("$(RES3)")'

result4: $(RES4)
	Rscript -e 'dget("$(RES4)")'

result5: $(RES5)
	Rscript -e 'dget("$(RES5)")'

result6: $(RES6)
	Rscript -e 'dget("$(RES6)")'

result7: $(RES7)
	Rscript -e 'dget("$(RES7)")'

result8: $(RES8)
	Rscript -e 'dget("$(RES8)")'

tar: $(NAME).tar.gz

# tar the replication code
# COPYFILE_DISABLE=1 to avoid MacOS adding ._ files to tarball
$(NAME).tar.gz:
	$(eval ICDTMP := $(shell mktemp -d))
	@mkdir "$(ICDTMP)/$(NAME)"
	cp * $(ICDTMP)/$(NAME); \
	olddir="$$PWD"; \
	cd $(ICDTMP); \
	COPYFILE_DISABLE=1 tar -cvz -f $(NAME).tar.gz \
	  --exclude=".*" \
	  --exclude=$(NAME).tar.gz \
	  --exclude="dput*.R" \
	  --exclude=result\*.rds $(NAME); \
	cd "$$olddir";	cp $(ICDTMP)/$(NAME).tar.gz "$$olddir"; rm -rf "$(ICDTMP)"

clean: cleantar
	rm -f $(ARTICLE).R
	rm -f $(README).html

cleantar:
	rm -f $(NAME).tar.gz

cleanbench:
	rm -f bench-versus-result* bench-versus-dput* \
	  dput-latest*.R dput-dated*.R result*.rds

dockerbuild:
	docker build -t icd-jss3447-replication .

dockerrun:
	docker run --rm -ti icd-jss3447-replication

dockerbash:
	docker run --rm -ti icd-jss3447-replication bash

.benchall: bench3 bench4 bench5 bench6 bench7 bench8

.resultall: result3 result4 result5 result6 result7 result8

.dockerall: dockerbash dockerrun dockerbuild

.cleanall: cleantar cleanbench clean

.PHONY: default .benchall .resultall deps all purl tar readme .dockerall .cleanall
