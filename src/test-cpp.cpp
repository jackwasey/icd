#include <Rcpp.h>
#include <testthat.h>
#include "is.h"

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

