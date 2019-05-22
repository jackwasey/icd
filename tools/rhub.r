#tools::compactPDF(gs_quality = "ebook")
# install arg --compact-docs to avoid rhub warning? somehow make part of usual build process? esp for pre-built vignettes

# other R CMD INSTALL options which might help cleaning up rhub output for ASAN ( which needs C++11 to install xml2 correctly):
# --configure-args --configure-vars (probably not useful)

# R CMD check

# --install-args=""


# https://github.com/r-hub/rhub/issues/273
# https://github.com/r-lib/xml2/issues/231
rhub::check_with_santizer(env_vars = c("CXX" = "g++ -std=c++11"))


# also need to NOT build vignette wrappers for pre-built vignettes, because
# pdfpages.sty is not available (on at least one platform)
