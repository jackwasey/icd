# valgrind icd10 searching
# ln -sf ~/.R/Makevars.valgrind Makevars
# ~/icd/tools/install_full.sh
# R --vanilla -d "valgrind --tool=callgrind --instr-atstart=no"
#
library(icd)

# try with and without ICD codes as a factor: need strings either way, but a
# factor we should not be converting to string and back, just work on the factor
# levels.

n <- 1e5
set.seed(1441)
dat <- data.frame(id = n + seq(n),
                  icd_code = sample(icd:::as_char_no_warn(icd10cm2016$code), n, replace = TRUE),
                  stringsAsFactors = FALSE)
system.time(pccc_dx <- comorbid_pccc_dx(dat))
