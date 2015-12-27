#include <Rcpp.h>
#include <testthat.h>
#include "is.h"
#include "util.h"

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
}
