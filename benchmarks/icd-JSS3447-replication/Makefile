#!/usr/bin/env make
# make --warn-undefined-variables --debug=b --print-data-base

# TODO: test on windows and other platforms with space in path name

# configurable:
NAME = icd-JSS3447-replication
DOCKER_TAG = icdjss3447repl
RES_DIR = results
RMD = efficiency-prebuilt
UPSFX = _UPDATED
README_NAME = README-replication
DEPS_LIB = depslib
DEPS_DONE = .depsdone.dummy
ARTICLE_FILENAME_BASE = Efficient_calculation_of_comorbidities_from_medical_codes

# derived:
ICD_VER := $(shell sed -n '/^Version: /s/Version: //p' ../../DESCRIPTION)
ICD_SRC_FILES := $(wildcard ../../R/*.R) \
	$(wildcard ../../src/*.h) \
	$(wildcard ../../src/*.cpp) \
	$(wildcard ../../src/*.c) \
	$(wildcard ../../src/Makevars*) \
	../../configure.ac \
	../../vignettes/*.Rmd \
	../../vignettes/*.Rnw \
	../../vignettes/*.bib \
	../../NEWS.md \
	../../README.Rmd \
	../../README.md \
	../../DESCRIPTION

ARTICLE_FILENAME = $(ARTICLE_FILENAME_BASE)$(UPSFX)
REPL_R = $(ARTICLE_FILENAME).R
ARTICLE_PDF = $(ARTICLE_FILENAME).pdf
SUB_TGZ = $(ARTICLE_FILENAME).tar.gz
ICD_SRC_TGZ = icd_$(ICD_VER).tar.gz
REPL_TGZ = $(ARTICLE_FILENAME)_code.tar.gz

# R
R_HOME := $(shell R RHOME)
R = "$(R_HOME)/bin/R"
RSCRIPT = "$(R_HOME)/bin/Rscript"
ifndef OS
	OS = ""
endif
ifeq ($(OS),Windows_NT)
	R := $(shell echo '$(R_HOME)\\bin\\R')
	RSCRIPT := $(shell echo '$(R_HOME)\\bin\\Rscript')
endif 

.PHONY: default ## a quick run of the quickest part of the benchmarking, to show it works
default: result3

# make the replications materials for JSS submission, without running heavy
# benchmarks, and run small benchmark to make finally sure it works.

replication_materials replmat repmat submission: $(SUB_TGZ) bench3

submission $(SUB_TGZ): $(ICD_SRC_TGZ) purl readme $(ARTICLE_PDF) bench3
	# $(RSCRIPT) bench-versus.R 3
	COPYFILE_DISABLE=1 tar -cvzf "$(SUB_TGZ)" \
					 medicalrisk_1.2.tar.gz \
					 comorbidity_0.1.1.tar.gz \
					 "$(ICD_SRC_TGZ)" \
					 "$(ARTICLE_PDF)"

$(ARTICLE_PDF):
	$(RSCRIPT) -e 'rmarkdown::render("../../vignettes/$(RMD).Rmd", output_dir = getwd(), output_file = "$(ARTICLE_PDF)")'


$(REPL_R) purl: ../../vignettes/$(RMD).Rmd
	$(RSCRIPT) -e 'knitr::purl("../../vignettes/$(RMD).Rmd", output = "$(REPL_R)")'

readme $(README_NAME).html $(README_NAME).pdf $(README_NAME).md: $(README_NAME).Rmd
	$(RSCRIPT) -e 'rmarkdown::render("$(README_NAME).Rmd", output_format = "all")'

.PHONY: deps
deps: $(DEPS_DONE)

# belt and braces - install dependencies function/script should check
$(DEPS_DONE):
	if ! test -d "$(DEPS_LIB)"; then \
	echo "creating $(DEPS_LIB)"; \
	mkdir "$(DEPS_LIB)"; \
	fi
	ICD_INSTALL_BENCH_DEPS_ON_SOURCE=yes ICD_BENCH_DEPS_LIB="$(DEPS_LIB)" $(RSCRIPT) install-dependencies.R ;
	touch $(DEPS_DONE)

RES3 = dput-latest-n-3.R
RES4 = dput-latest-n-4.R
RES5 = dput-latest-n-5.R
RES6 = dput-latest-n-6.R
RES7 = dput-latest-n-7.R
RES8 = dput-latest-n-8.R
RES9 = dput-latest-n-9.R

$(RES3): deps
	$(RSCRIPT) bench-versus.R 3

$(RES4): deps
	$(RSCRIPT) bench-versus.R 4

$(RES5): deps
	$(RSCRIPT) bench-versus.R 5

$(RES6): deps
	$(RSCRIPT) bench-versus.R 6

$(RES7): deps
	$(RSCRIPT) bench-versus.R 7

$(RES8): deps
	$(RSCRIPT) bench-versus.R 8

$(RES9): deps
	$(RSCRIPT) bench-versus.R 9

bench3: $(RES3) bench-versus.R
bench4: $(RES4) bench-versus.R
bench5: $(RES5) bench-versus.R
bench6: $(RES6) bench-versus.R
bench7: $(RES7) bench-versus.R
bench8: $(RES8) bench-versus.R
bench9: $(RES9) bench-versus.R

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

result9: $(RES9)
	Rscript -e 'dget("$(RES9)")'

# tar just the replication code
# COPYFILE_DISABLE=1 to avoid MacOS adding ._ files to tarball
$(REPL_TGZ): purl readme
	$(eval ICDTMP := $(shell mktemp -d))
	@mkdir "$(ICDTMP)/$(NAME)"
	cp * $(ICDTMP)/$(NAME); \
		olddir="$$PWD"; \
		cd $(ICDTMP); \
		COPYFILE_DISABLE=1 tar -cvzf $(NAME).tar.gz \
		--exclude=".*" \
		--exclude="*.tex" \
		--exclude=$(NAME).tar.gz \
		--exclude=$(SUB_TGZ) \
		--exclude=icd_3.\*tar.gz \
		--exclude="*pdf" \
		--exclude="dput*.R" \
		--exclude="$(RES_DIR)/dput*.R" \
		--exclude="$(README).Rmd" \
		--exclude=result\*.rds $(NAME); \
		cd "$$olddir"; \
		cp $(ICDTMP)/$(NAME).tar.gz "$$olddir"; \
		rm -rf "$(ICDTMP)"

# R CMD build always makes a tar.gz file, even on windows
build_pkg: $(ICD_SRC_TGZ)
$(ICD_SRC_TGZ): $(ICD_SRC_FILES)
	R CMD build --md5 --log --resave-data=xz --compact-vignettes=gs+qpdf ../..

install_icd_in_temp_lib_dbg: $(ICD_SRC_TGZ)
	$(R) CMD INSTALL \
		--preclean \
		--debug \
		--library="$(DEPS_LIB)" \
		--use-vanilla \
		--no-clean-on-error \
		"$(ICD_SRC_TGZ)"

install_icd_in_temp_lib: $(ICD_SRC_TGZ)
	$(R) CMD INSTALL --clean --library="$(DEPS_LIB)" "$(ICD_SRC_TGZ)"

.PHONY: clean
clean:
	rm -f $(REPL_R) $(ARTICLE_FILENAME).* icd-.*build.log
	rm -rf $(ARTICLE_FILENAME)_files/
	rm -f $(README).html $(README).md $(README).pdf
	rm -f $(NAME).tar.gz icd_*.tar.gz $(NAME)-JSS*.tar.gz

.PHONY: cleanbench
cleanbench:
	rm -f bench-versus-result* bench-versus-dput* \
		dput-latest*.R dput-dated*.R \
		result*.rds \
		results/dput-latest*.R results/dput-dated*.R \
		results/result*.rds

.PHONY: cleandeps
cleandeps:
	rm -f $(DEPS_DONE)
	rm -rf $(DEPS_LIB)

style: install-dependencies.R bench-versus.R find-comorbidity-cutoff.R
	$(RSCRIPT) -e 'styler::style_file(c("install-dependencies.R", "bench-versus.R", "util-bench.R", "find-comorbidity-cutoff.R"))'

.PHONY: destroy
destroy: cleandeps cleanbench clean

dockerbuild: Dockerfile install-dependencies.R util-bench.R find-comorbidities-cutoff.R bench-versus.R Makefile
	docker build -t $(DOCKER_TAG) .

docker: dockerbuild
	docker run --rm -ti $(DOCKER_TAG)

# following are phony targets
.benchall: bench3 bench4 bench5 bench6 bench7 bench8 bench9
.resultall: result3 result4 result5 result6 result7 result8 result9

.PHONY: all
all: result3

.PHONY: test
test: $(ICD_SRC_TGZ)
	$(R) CMD check --no-build-vignettes $(ICD_SRC_TGZ)
