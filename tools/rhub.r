#!/usr/bin/env Rscript
# tools::compactPDF(gs_quality = "ebook")
# install arg --compact-docs to avoid rhub warning? somehow make part of usual build process? esp for pre-built vignettes

# other R CMD INSTALL options which might help cleaning up rhub output for ASAN ( which needs C++11 to install xml2 correctly):
# --configure-args --configure-vars (probably not useful)

# R CMD check

# --install-args=""

# original effort at invoking with compiler flags:
# rhub::check_with_santizer(env_vars = c("CXX" = "g++ -std=c++11"))
# rhub::check_with_sanitizers(env_vars = c(GNUMAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes", MAKEFLAGS = "CXXFLAGS+=-std=c++11 \\ -Wno-ignored-attributes"))
# rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))


# also need to NOT build vignette wrappers for pre-built vignettes, because
# pdfpages.sty may still not available (on at least one platform)

# MAKEFLAGS="CXXFLAGS+=-std=c++14\ -Wno-ignored-attributes CXX11FLAGS+=-std=c++14\ -O3" GNUMAKEFLAGS="CXX11FLAGS+=-std=c++11\ -Wno-ignored-attributes" R CMD
# The following seems to work. I added -Wno-ignored-attributes just to reduce the massive Eigen compilation noise.

# Rscript -e 'rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))'
# great care with makeflags space-separation of each setting, but must quote or
# double quote any spaces within a setting.
# really just want to silence massive Eigen compilation output

# From WRE:
# Flags that are already set (for example in file etcR_ARCH/Makeconf) can be
# overridden by the environment variable MAKEFLAGS (at least for systems using a
# POSIX-compliant make), as in (Bourne shell syntax) MAKEFLAGS="CFLAGS=-O3" R
#  CMD SHLIB *.c It is also possible to set such variables in personal
# Makevars files, which are read after the local Makevars and the system
# makefiles or in a site-wide Makevars.site file. See Customizing package
# compilation in R Installation and Administration,


setwd(Sys.getenv("ICD_HOME"))
rhub_env <- read.delim(
  comment.char = "#",
  sep = "=",
  file = "tools/env/rhub",
  header = FALSE,
  strip.white = TRUE,
  blank.lines.skip = TRUE,
  quote = '"',
  col.names = c(
    "name",
    "value"
  ),
  row.names = 1
)
rhe <- c()
for (n in rownames(rhub_env)) {
  rhe[n] <- rhub_env[n, "value"]
}

sanitize <- FALSE
if (sanitize) {
  rhub::check_with_sanitizers(
    env_vars = c(
      MAKEFLAGS = "CXX11FLAGS+=-w CXXFLAGS+=-w",
      GNUMAKEFLAGS = "CXX11FLAGS+=-w CXXFLAGS+=-w"
    )
  )
  # rhub::check_on_windows(env_vars = rhe)
}

rhub_res <- list()
plats <- c(
  # "macos-highsierra-release-cran",
  # "linux-x86_64-rocker-gcc-san",
  # "fedora-clang-devel",
  "debian-gcc-patched",
  # "windows-x86_64-patched",
  # "ubuntu-gcc-devel",
  # "solaris-x86-patched",
  NULL
)

makeflags <- c("CXXFLAGS"="-w",
            "CXX11FLAGS"="-w",
            "CFLAGS"="-w")
.makeflags <- paste(names(makeflags), makeflags, sep='=', collapse="\\ ")
configure_args <- c("--enable-icd-shutup")
configure_vars <- c(MAKEFLAGS = .makeflags,
                    "_R_CHECK_DEPENDS_ONLY_"="true")
install_args <- c("--no-docs",
                  .configure_args,
                  .configure_vars)
check_args <- c(
  "--ignore-vignettes",
  "--no-manual",
  "--no-codoc",
  "--no-vignettes",
  "--no-build-vignettes",
  "--install-args='--no-build-vignettes --configure-args=\"--enable-icd-shutup\" --configure-vars=\"MAKEFLAGS=\\\"CXXFLAGS=-w CXX11FLAGS=-w _R_CHECK_DEPENDS_ONLY_=TRUE\\\"\"'")

rhub_res <- rhub::check(
  check_args = .checks_args,
    NULL
  ),
  env_vars = rhe,
  platform = plats
)
