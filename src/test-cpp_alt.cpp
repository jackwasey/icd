#include "config.h"
#include "icd_types.h"
#include "local.h"
#include <Rcpp.h>
#include "manip_alt.h"

// nocov start

#ifdef ICD_CATCH
#include <testthat.h>

context("test alternate zero-adding code") {
  test_that("when a code would be made ambiguous, don't change it") {
    expect_true(Rcpp::as<std::string>(icd9AddLeadingZeroes_alt_Direct("E010", true)[0]) == "E010");
  }
  test_that("short E codes work") {
    expect_true(Rcpp::as<std::string>(icd9AddLeadingZeroes_alt_Direct("E1", true)[0]) == "E001");
    //Rcpp::String e1 = icd9AddLeadingZeroes_alt_ShortSingle("E1");
    //expect_true(Rcpp::as<std::string>(CV::create(e1)) == "E001");
  }
}

// endif have testthat ICD_CATCH
#endif

// nocov end
