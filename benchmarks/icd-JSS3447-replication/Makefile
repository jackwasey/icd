R_HOME := "$(shell R RHOME)"
ifeq ($(OS),Windows_NT)
	R := $(shell echo '$(R_HOME)\\bin\\R')
	RSCRIPT := $(shell echo '$(R_HOME)\\bin\\Rscript')
else
	R := $(shell echo '$(R_HOME)/bin/R')
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
DTAG := icdjss3447repl
RMD := efficiency-prebuilt
ARTICLE := Efficient_calculation_of_comorbidities_from_medical_codes
UPDATED := $(ARTICLE)_UPDATED
ARTICLEPDF := $(UPDATED).pdf
README := README-replication
SUB := JSS3447_updated_article_and_replication.tar.gz
ICD_VER := $(shell sed -n '/^Version: /s/Version: //p' ../../DESCRIPTION)
ICD_SRC_PKG := icd_$(ICD_VER).tar.gz

# a quick run of the quickest part of the benchmarking, to show it works
default: result3

# make the replications materials for JSS submission, without running heavy benchmarks, but run small benchmark to make finally sure it works...
replicationmaterials replmat repmat submission: $(SUB) bench3

$(SUB): $(NAME).tar.gz purl readme $(ARTICLEPDF) $(ICD_SRC_PKG)
	$(RSCRIPT) bench-versus.R 3
	COPYFILE_DISABLE=1 tar -cvzf "$(SUB)" \
	$(NAME).tar.gz \
	medicalrisk_1.2.tar.gz \
	comorbidity_0.1.1.tar.gz \
	$(ICD_SRC_PKG) \
	$(ARTICLEPDF)

$(ARTICLEPDF):
	$(RSCRIPT) -e 'rmarkdown::render("../../vignettes/$(RMD).Rmd", output_dir = getwd(), output_file = "$(ARTICLEPDF)")'

../../vignettes/$(RMD).Rmd: ;

$(ARTICLE).R: ../../vignettes/$(RMD).Rmd
	$(RSCRIPT) -e 'knitr::purl("../../vignettes/$(RMD).Rmd", output = "$(ARTICLE).R")'

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

# tar the replication code
# COPYFILE_DISABLE=1 to avoid MacOS adding ._ files to tarball
$(NAME).tar.gz: purl readme
	$(eval ICDTMP := $(shell mktemp -d))
	@mkdir "$(ICDTMP)/$(NAME)"
	cp * $(ICDTMP)/$(NAME); \
	olddir="$$PWD"; \
	cd $(ICDTMP); \
	COPYFILE_DISABLE=1 tar -cvzf $(NAME).tar.gz \
	  --exclude=".*" \
	  --exclude="*.tex" \
	  --exclude=$(NAME).tar.gz \
	  --exclude=$(SUB) \
	  --exclude=icd_3.\*tar.gz \
	  --exclude="*pdf" \
	  --exclude="dput*.R" \
	  --exclude="$(README).Rmd" \
	  --exclude=result\*.rds $(NAME); \
	cd "$$olddir"; \
	cp $(ICDTMP)/$(NAME).tar.gz "$$olddir"; \
	rm -rf "$(ICDTMP)"

$(ICD_SRC_PKG): ../../DESCRIPTION
	R CMD build --compact-vignettes=gs+qpdf ../..

clean:
	rm -f $(ARTICLE).R
	rm -rf $(UPDATED)_files/
	rm -f $(UPDATED).*
	rm -f $(README).html
	rm -f .deps
	rm -f $(NAME).tar.gz icd_*.tar.gz JSS*.tar.gz

cleanbench:
	rm -f bench-versus-result* bench-versus-dput* \
	  dput-latest*.R dput-dated*.R result*.rds

dockerbuild: Dockerfile install-dependencies.R bench-versus.R Makefile
	docker build -t $(DTAG) .

docker: dockerbuild
	docker run --rm -ti $(DTAG)

Makefile: ;

Dockerfile: ;

../../DESCRIPTION: ;

.benchall: bench3 bench4 bench5 bench6 bench7 bench8

.resultall: result3 result4 result5 result6 result7 result8

.PHONY: default deps all purl readme docker dockerbuid clean cleanbench submission replmat
