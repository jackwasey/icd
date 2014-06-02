library(jh)
library(profr)
library(testthat)
library(futile.logger)

old_threshold <- flog.threshold(FATAL)
test_check("jh")
flog.threshold(old_threshold)

# check test code coverage
#jhfuns <- lsp(jhdata) # see function in util which lists contents of a package
#jhprofr <- profr(
#  test_dir("/home/jack/tests/testthat/", environment()),
#  quiet=F
#)
#everytestedfun <- unique(jhprofr$f)
#jhtested <- jhfuns[jhfuns %in% everytestedfun]
#jhuntested <- jhfuns[jhfuns %nin% everytestedfun]
#flog.info("Percentage test coverage is %.1f", 100*length(jhtested)/length(jhfuns))
#flog.info("tested functions are:", jhtested, capture=T)
#flog.warn("untested functions are:", jhuntested, capture=T)

