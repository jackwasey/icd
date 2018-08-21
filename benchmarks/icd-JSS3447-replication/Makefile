
# this should work on unices and Windows
HOST := $(shell hostname)
RES3 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 3, Sys.info()["nodename"], sep = "-"), ".R"))')
RES7 := $(shell Rscript -e 'cat(paste0(paste("bench-versus-dput", 7, Sys.info()["nodename"], sep = "-"), ".R"))')

# replication of benchmark results in icd JSS submission
purl: ; Rscript -e 'knitr::purl(system.file("vignettes", "Efficient_calculation_of_comorbidities_from_medical_codes.Rmd", package = "icd")'

deps:
	Rscript install-dependencies.R

bench:
	Rscript bench-versus.R

.PHONY $(RES3)
$(RES3):
	Rscript bench-versus.R 3

quickbench: $(RES3)
	Rscript bench-versus.R 3

fullbench: $(RES7)
	Rscript bench-versus.R 7

# .PHONY bench deps purl
