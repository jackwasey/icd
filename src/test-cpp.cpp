#include <Rcpp.h>
#include <testthat.h>
#include "is.h"
#include "util.h"
#include "convert.h"
#include "../inst/include/icd9.h"

context("C++ Unit Test") {
  test_that("two plus two is four") {
    int result = 2 + 2;
    expect_true(result == 4);
  }
}

context("test internal 'is' functions") {
  test_that("is a basic") {
    std::vector<std::string> v;
    v.push_back("V10");
    std::vector<bool> result = icd9IsA(v, "V", false);
    for (std::vector<bool>::iterator i = result.begin(); i != result.end(); ++i) {
      expect_true(*i);
    }
  }
}

context("test get OMP max threads") {
  test_that("max threads give a semi-sensible number") {
    int i = getOmpMaxThreads();
    expect_true(i >= 0);
  }
}

context("test get OMP threads") {
  test_that("threads give a semi-sensible number") {
    int i = getOmpThreads();
    expect_true(i >= 0);
  }
}

// context("test randomMajorCpp") {
//   test_that("random majors looks okay") {
//     Rcpp::NumericVector res = randomMajorCpp(5L);
//     expect_true(res.size() == 5);
//     expect_true(all(res >=0));
//   }
// }

context("icd9ShortToPartsCpp") {
  test_that("icd9ShortToPartsCpp gives NA value") {
    Rcpp::List out = icd9ShortToPartsCpp("E12345678", "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));

  }

  test_that("icd9ShortToPartsCpp multiple inptus gives multiple NA values") {
    Rcpp::CharacterVector cv = Rcpp::CharacterVector::create("E3417824921",
                                                             "E375801347",
                                                             "E8319473422");
    Rcpp::List out = icd9ShortToPartsCpp(cv, "");

    Rcpp::CharacterVector j = out["major"];
    Rcpp::CharacterVector n = out["minor"];

    expect_true(Rcpp::CharacterVector::is_na(j[0]));
    expect_true(Rcpp::CharacterVector::is_na(n[0]));
    expect_true(Rcpp::CharacterVector::is_na(j[1]));
    expect_true(Rcpp::CharacterVector::is_na(n[1]));
    expect_true(Rcpp::CharacterVector::is_na(j[2]));
    expect_true(Rcpp::CharacterVector::is_na(n[2]));

  }
}

// context("Rcpp exports") {
//   test_that("Rcpp exports are valid") {
//
//     int i = icd9_RcppExport_validate("icd9");
//     expect_that(i > 0);
//   }
// }
