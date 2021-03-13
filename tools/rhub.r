#!/usr/bin/env Rscript

if (!exists("do_local")) do_local <- TRUE
if (!exists("do_remote")) do_remote <- FALSE
if (!exists("dry_run")) dry_run <- TRUE
if (!exists("sanitize")) sanitize <- FALSE
if (!exists("rchk")) rchk <- FALSE
tmpdir <- "/tmp/icd-rhub"
dir.create(tmpdir, showWarnings = FALSE)
if (!dir.exists(tmpdir)) stop(tmpdir, " could not be created.")

# tools::compactPDF(gs_quality = "ebook")
# install arg --compact-docs to avoid rhub warning? somehow make part of usual build process? esp for pre-built vignettes

# other R CMD INSTALL options which might help cleaning up rhub output for ASAN ( which needs C++11 to install xml2 correctly):
# --configure-args --configure-vars

# original effort at invoking with compiler flags:
# rhub::check_with_santizer(env_vars = c("CXX" = "g++ -std=c++11"))
# rhub::check_with_sanitizers(env_vars = c(GNUMAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes", MAKEFLAGS = "CXXFLAGS+=-std=c++11 \\ -Wno-ignored-attributes"))
# rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))

## See my own posted comment: https://github.com/r-hub/rhub/issues/273
# Rscript -e 'rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))'

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

## rhub and rcmdcheck are basis: may prefer to use, but does abstract further,
## making it potentially harder to set specific details, in this case with the
## goal of making r-hub builds much faster and leaner, focusing on the actual
## code and tests, and removing 10,000 lines of Eigen compiler warnings, due to
## a CRAN requirement not to suppress even irrelevant ones.
##
## rhub, rcmdcheck, and R CMD install|build|check all have different semantics
## and quoting requirements for R and shell layers.

setwd(Sys.getenv("ICD_HOME", unset = getwd()))
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
rm(rhe_env)

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
  "ubuntu-rchk",
  # "macos-highsierra-release-cran",
  # "linux-x86_64-rocker-gcc-san",
  # "fedora-clang-devel",
  "debian-gcc-patched",
  # "windows-x86_64-patched",
  # "ubuntu-gcc-devel",
  # "solaris-x86-patched",
  ifelse(sanitize, "linux-x86_64-rocker-gcc-san", NULL),
  ifelse(rchk, "ubuntu-rchk", NULL),
)

shutup <- "-w"
makeflag_vars <- c(
  "CXXFLAGS" = shutup,
  "CXX11FLAGS" = shutup,
  "CXX14FLAGS" = shutup,
  "CXX17FLAGS" = shutup,
  "CXX20FLAGS" = shutup,
  "CFLAGS" = shutup
)

..makeflag_vars <-
  paste(names(makeflag_vars), makeflag_vars, sep = "=", collapse = "\\ ")
# ..makeflag_vars <- gsub("\\", "\\\\", .makeflag_vars)
.makeflag_vars <- paste0("\"", ..makeflag_vars, "\"")

configure_args <- c("--enable-icd-shutup")

configure_vars <- c(
  MAKEFLAGS = .makeflag_vars,
  "_R_CHECK_DEPENDS_ONLY_" = "true"
)
.configure_vars <- paste(names(configure_vars), configure_vars, sep = "=", collapse = "\\ ")

build_args <- c(
  "--no-build-vignettes",
  "--no-resave-data",
  "--no-manual"
)

install_args <- c(
  # "--preclean",
  # "--debug",
  # "--no-docs",
  # "--install-tests",
  paste0("--configure-args=", configure_args),
  # paste0("--configure-vars=", .configure_vars),
  NULL
)

check_args <- c(
  sprintf(
    "--install-args='%s'",
    paste(install_args, collapse = " ")
  ),
  "--no-manual",
  "--no-codoc",
  "--no-vignettes",
  "--no-build-vignettes",
  "--ignore-vignettes",
  NULL
)

if (icd:::.verbose()) {
  symbol_names <- c(
    "makeflags",
    "configure_args",
    "configure_vars",
    "build_args",
    "install_args",
    "check_args"
  )
  for (s in symbol_names) {
    for (dot_prefix in c("", ".")) {
      nm <- paste0(dot_prefix, s)
      if (exists(nm)) {
        message("print(", nm, ")")
        print(get(nm))

        message("cat(", nm, ")")
        cat(get(nm))
        cat("\n\n")
      }
    }
  }
  withr::with_envvar(
    c(
      GITHUB_USERNAME = "jackwasey",
      EMAIL = "jack@jackwasey.com"
    ),
    whoami::whoami()
  )
  withr::with_envvar(
    c(
      GITHUB_USERNAME = "jackwasey",
      EMAIL = NULL
    ),
    whoami::whoami()
  )
  rhub:::get_maintainer_email(".")
  rhub::list_validated_emails()
}
if (!dry_run) {
  if (do_local) {
    ## and/or my shell scripts

    ## pre-build to get quicker and more verbose compilation
    ##
    ## with_makevars is an option here, but to better get in line with rhub, use
    ## with_envvar?
    if (file.exists(file.path("~", ".R", "Makevars"))) {
      stop("~/.R/Makevars exists")
    }
    pkgbuild::clean_dll()
    pkgbuild::build(
      dest_path = tmpdir,
      vignettes = FALSE,
      manual = FALSE,
      quiet = FALSE,
      needs_compilation = TRUE,
      compile_attributes = TRUE,
      binary = TRUE,
      ## n.b. if binary is set, then this should be INSTALL args
      args = install_args
    )
    res_rcc <- rcmdcheck::rcmdcheck(
      quiet = FALSE,
      check_dir = tmpdir,
      error_on = "warning",
      args = check_args,
      build_args = build_args
    )
    message("res_rcc")
  } else {
    ## pre-requisites for r-hub email validation. whoami uses git global, so
    ## cannot override by changing the repo .gitconfig.
    withr::with_envvar(
      c(
        GITHUB_USERNAME = "jackwasey",
        EMAIL = "jack@jackwasey.com"
      ),
      {
        res_rh <- rhub::check(
          check_args = check_args,
          env_vars = rhe,
          platform = plats
        )
      }
    )
    message("res_rh")
  }
  message("results in: ", tmpdir)
} else {
  message("Dry run complete")
}
