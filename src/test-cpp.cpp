#ifdef ICD_CATCH
#include "appendMinor.hpp"
#include "convert.hpp"
#include "convert10.hpp"
#include "icd_types.hpp"
#include "is.hpp"
#include "local.hpp"
#include "manip.hpp"
#include "refactor.hpp"
#include "util.hpp"

#include <testthat.h>
using namespace Rcpp;
context("internal 'is' functions") {
  test_that("is n") {
    VecStr v;
    v.push_back("10");
    v.push_back("0");
    v.push_back("10.");
    v.push_back("10.9");
    v.push_back("100.99");
    v.push_back("9999");
    v.push_back("1");
    std::vector<bool> result  = icd9_is_n_rcpp(v);
    std::vector<bool> result2 = icd9_is_v_rcpp(v);
    std::vector<bool> result3 = icd9_is_e_rcpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
  test_that("is v") {
    VecStr v;
    v.push_back("V10");
    v.push_back("V0");
    v.push_back("V10.");
    v.push_back("V10.9");
    v.push_back("V99.99");
    v.push_back("V9999");
    v.push_back("v1");
    std::vector<bool> result2 = icd9_is_n_rcpp(v);
    std::vector<bool> result  = icd9_is_v_rcpp(v);
    std::vector<bool> result3 = icd9_is_e_rcpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
  test_that("is e basic") {
    VecStr v;
    v.push_back("E999");
    v.push_back("E0");
    v.push_back("E999.");
    v.push_back("E999.9");
    v.push_back("E9999");
    v.push_back("e000");
    std::vector<bool> result3 = icd9_is_n_rcpp(v);
    std::vector<bool> result2 = icd9_is_v_rcpp(v);
    std::vector<bool> result  = icd9_is_e_rcpp(v);
    for (std::vector<bool>::size_type i = 0; i != result.size(); ++i) {
      expect_true(result[i]);
      expect_false(result2[i]);
      expect_false(result3[i]);
    }
  }
}

context("icd9ShortToParts") {
  test_that("icd9ShortToParts gives NA value") {
    List out = icd9ShortToParts("E12345678", "");
    CV j     = out["mjr"];
    CV n     = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
  }
  test_that("icd9ShortToParts multiple inptus gives multiple NA values") {
    CV cv    = CV::create("E3417824921", "E375801347", "E8319473422");
    List out = icd9ShortToParts(cv, "");
    CV j     = out["mjr"];
    CV n     = out["mnr"];
    expect_true(CV::is_na(j[0]));
    expect_true(CV::is_na(n[0]));
    expect_true(CV::is_na(j[1]));
    expect_true(CV::is_na(n[1]));
    expect_true(CV::is_na(j[2]));
    expect_true(CV::is_na(n[2]));
  }
}

context("MajMin to code") {
  test_that("differing lengths gives error") {
    CV mj = CV::create("100");
    CV mn = CV::create("01");
    CV rs = CV::create("10001");
    expect_true(is_true(all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("101", "102");
    mn = CV::create("01", "02");
    rs = CV::create("10101", "10202");
    expect_true(is_true(all(icd9MajMinToCode(mj, mn, true) == rs)));
    mj = CV::create("100");
    mn = CV::create("01", "02");
    mn = CV::create("01", "02");
    mj = CV::create("100", "101", "102");
  }
}

context("add leading zeroes to major") {
  test_that("when major len is 0, result is empty, non-std") {
    expect_true(icd9AddLeadingZeroesMajorSingle("") == NA_STRING);
  }
  test_that("when major len is 0, result is empty, std") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("") == "");
  }
  test_that("when major len is 2, result is ok") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("01") == "001");
  }
  test_that("E code with two char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E1") == "E001");
  }
  test_that("E code with three char major works") {
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E01") == "E001");
  }
  test_that("0") {
    expect_true(icd9AddLeadingZeroesMajorSingle("0") == "000");
    expect_true(icd9AddLeadingZeroesMajorSingle("0") == "000");
  }
  test_that("00") {
    expect_true(icd9AddLeadingZeroesMajorSingle("00") == "000");
    expect_true(icd9AddLeadingZeroesMajorSingle("00") == "000");
  }
  test_that("01") {
    expect_true(icd9AddLeadingZeroesMajorSingle("01") == "001");
    expect_true(icd9AddLeadingZeroesMajorSingleStd("01") == "001");
  }
  test_that("V1") {
    expect_true(icd9AddLeadingZeroesMajorSingle("V1") == "V01");
    expect_true(icd9AddLeadingZeroesMajorSingleStd("V1") == "V01");
  }
  test_that("E2") {
    expect_true(icd9AddLeadingZeroesMajorSingle("E2") == "E002");
    expect_true(icd9AddLeadingZeroesMajorSingleStd("E2") == "E002");
  }
  test_that("1234 too long major") {
    expect_true(icd9AddLeadingZeroesMajorSingle("1234") == NA_STRING);
    expect_true(icd9AddLeadingZeroesMajorSingleStd("1234") == "");
  }
}

context("refactor") {
  IntegerVector f;
  IntegerVector f2;
  CV new_levels;
  CV empty_levels(0);
  CV a_level = CV::create("a");
  CV b_level = CV::create("b");
  CV j;
  IntegerVector res;
  test_that("one factor level") {
    f                = {1};
    f.attr("levels") = "old_level";
    f.attr("class")  = "factor";
    res              = refactor(f, empty_levels, false);
    expect_true(IntegerVector::is_na(res[0]));
    j = res.attr("levels");
    expect_true(j.size() == 0);
  }
  test_that("empty factor levels") {
    f                = IntegerVector();
    f.attr("levels") = empty_levels;
    f.attr("class")  = "factor";
    res              = refactor(f, empty_levels, false);
    expect_true(res.size() == 0);
    j = res.attr("levels");
    expect_true(j.size() == 0);
  }
  test_that("NA with no NA factor levels") {
    f                = {NA_INTEGER};
    f.attr("levels") = a_level;
    f.attr("class")  = "factor";
    res              = refactor(f, a_level, true);
    expect_true(IntegerVector::is_na(res[0]));
    j = res.attr("levels");
    expect_true(is_true(all(j == a_level)));
  }
  test_that("2 NAs, no NA factor levels") {
    f                = {NA_INTEGER, NA_INTEGER};
    f.attr("levels") = a_level;
    f.attr("class")  = "factor";
    res              = refactor(f, b_level, true);
    expect_true(IntegerVector::is_na(res[0]));
    expect_true(IntegerVector::is_na(res[1]));
    j = res.attr("levels");
    expect_true(is_true(all(j == b_level)));
  }
  test_that("2 NAs, with NA factor levels") {
    f                = {NA_INTEGER, NA_INTEGER};
    f.attr("levels") = CV::create("b", "c");
    f.attr("class")  = "factor";
    new_levels       = CV::create("a", "NA_STRING");
    new_levels[1]    = NA_STRING;
    res              = refactor(f, new_levels, false);
    expect_false(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_true(is_true(all(res == 2)));
    j = res.attr("levels");
    DEBUG_VEC(j);
    expect_true(CV::is_na(j[1]));
  }
  /* // skipping because not implemented or needed yet.
   test_that("don't drop NA values and levels") {
   f = { NA_INTEGER, 1, 3, NA_INTEGER };
   new_levels = CV::create("a", "c", "NA_STRING");
   new_levels[2] = NA_STRING;
   f.attr("levels") = new_levels;
   f.attr("class") = "factor";
   new_levels= CV::create("a", "NA_STRING");
   new_levels[1] = NA_STRING;
   res = refactor(f, new_levels, false);
   expect_false(IntegerVector::is_na(res[0]));
   expect_false(IntegerVector::is_na(res[1]));
   expect_false(IntegerVector::is_na(res[2]));
   IntegerVector expected_vec = IntegerVector::create(2, 1, 2, 2);
   expect_true(is_true(all(res == expected_vec)));
   j = res.attr("levels");
   DEBUG_VEC(j);
   expect_true(CV::is_na(j[1]));
   } */
  test_that("do drop NA values and levels") {
    f                = {NA_INTEGER, 1, 3, NA_INTEGER};
    CV old_levels    = CV::create("a", "c", "NA_STRING");
    old_levels[2]    = NA_STRING;
    f.attr("levels") = old_levels;
    f.attr("class")  = "factor";
    new_levels       = CV::create("a", "NA_STRING");
    new_levels[1]    = NA_STRING;
    res              = refactor_narm(f, new_levels);
    expect_false(IntegerVector::is_na(res[0]));
    IntegerVector expected_vec = {1};
    DEBUG_VEC(res);
    expect_true(is_true(all(res == expected_vec)));
    j = res.attr("levels");
    DEBUG_VEC(j);
    expect_false(CV::is_na(j[0]));
    expect_true(is_true(all(j == CV::create("a"))));
  }
  test_that("new NA level") {
    f                = {NA_INTEGER};
    f.attr("levels") = CV::create("a");
    f.attr("class")  = "factor";
    CV na_level(1);
    na_level[0] = NA_STRING;
    res         = refactor(f, na_level, false);
    expect_true(res[0] == 1);
  }
  test_that("factor self works") {
    f                = {1, 2};
    new_levels       = CV::create("a", "b");
    f.attr("levels") = new_levels;
    f.attr("class")  = "factor";
    res              = refactor(f, new_levels, false);
    expect_true(is_true(all(f == res)));
    bool levels_equal = is_true(all(new_levels == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor simple re-order") {
    f                 = {1, 2};
    f.attr("levels")  = CV::create("a", "b");
    f.attr("class")   = "factor";
    new_levels        = CV::create("b", "a");
    f2                = {2, 1};
    f2.attr("levels") = new_levels;
    f2.attr("class")  = "factor";
    res               = refactor(f, new_levels, false);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal =
      is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order") {
    f                 = {1, 3, 2};
    f.attr("levels")  = CV::create("a", "b", "c");
    f.attr("class")   = "factor";
    f2                = {3, 1, 2};
    new_levels        = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class")  = "factor";
    res               = refactor(f, new_levels, false);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal =
      is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra") {
    f                 = {1, 3, 2};
    f.attr("levels")  = CV::create("a", "b", "c", "d");
    f.attr("class")   = "factor";
    f2                = {3, 1, 2};
    new_levels        = CV::create("c", "b", "a");
    f2.attr("levels") = new_levels;
    f2.attr("class")  = "factor";
    res               = refactor(f, new_levels, false);
    expect_true(is_true(all(f2 == res)));
    bool levels_equal =
      is_true(all(((CV)f2.attr("levels")) == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
  }
  test_that("factor complex re-order w extra and missing") {
    f                 = {1, 3, 2};
    f.attr("levels")  = CV::create("a", "b", "c", "d");
    f.attr("class")   = "factor";
    f2                = {NA_INTEGER, 1, 2};
    new_levels        = CV::create("c", "b", "e");
    f2.attr("levels") = new_levels;
    f2.attr("class")  = "factor";
    res = refactor(f, new_levels, false); // keep NA values (but not in levels)
    DEBUG_VEC(res);
    expect_true(res.size() == 3);
    expect_true(res[0] == f2[0]);
    expect_true(res[1] == f2[1]);
    expect_true(res[2] == f2[2]);
    expect_true(IntegerVector::is_na(res[0]));
    expect_false(IntegerVector::is_na(res[1]));
    expect_false(IntegerVector::is_na(res[2]));
    bool levels_equal = is_true(all(new_levels == ((CV)res.attr("levels"))));
    expect_true(levels_equal);
    std::string cl         = f2.attr("class");
    std::string factor_str = "factor";
    expect_true(cl == factor_str);
    // expect_true(is_true(all(res == f2))); // doesn't work because NA = NA is
    // NA, not true
  }
  test_that("dropping NAs makes vector shorter") {
    f                 = {1, 2};
    f.attr("levels")  = CV::create("a", "b", "c", "d");
    f.attr("class")   = "factor";
    f2                = {1};
    new_levels        = CV::create("c", "b", "e");
    f2.attr("levels") = new_levels;
    f2.attr("class")  = "factor";
    res               = refactor_narm(f, new_levels);
    DEBUG_VEC(res);
    expect_false(IntegerVector::is_na(res[0]));
    expect_true(res[0] == 2);
    expect_true(res.size() == 1);
    expect_true(is_true(all(((CV)res.attr("levels")) == new_levels)));
  }
}
context("ICD10 short to parts") {
  test_that("icd10 short to parts handles NA") {
    const CharacterVector x = CharacterVector::create(NA_STRING);
    DataFrame res           = icd10ShortToParts(x);
    // expect_true(((CV)res[0])[0] == NA_STRING);
    // expect_true(((CV)res[0])[1] == NA_STRING);
  }
  test_that("too short, but not empty") {
    const CharacterVector x = CharacterVector::create("V1", "B", "C10");
    DataFrame r             = icd10ShortToParts(x);
    expect_true(r.size() == 2);
    expect_true(r.nrow() == x.size());
    for (auto i = 0; i != x.size() && i != r.size(); ++i) {
      auto cd      = x[i];
      CV c         = CharacterVector::create(cd, "");
      DataFrame r0 = icd10ShortToParts(CV::create(cd));
      expect_true(r0.size() == 2);
      expect_true(r0.nrow() == 1);
      CV row;
      row = getDataFrameStringRow(r0, 0);
      expect_true(strVecEqual(row, c));
      row = getDataFrameStringRow(r, i);
      expect_true(strVecEqual(row, c));
    }
  }
  test_that("empty") {
    CharacterVector x = CharacterVector::create("");
    DataFrame res     = icd10ShortToParts(x);
    CV mj             = res[0];
    CV mn             = res[1];
    String smj        = mj[0];
    String smn        = mn[0];
    expect_true(smj == NA_STRING);
    expect_true(smn == NA_STRING);
    expect_true(is_true(all(is_na((CV)res[0]))));
    expect_true(is_true(all(is_na((CV)res[1]))));
  }
}

context("compare/sort/order ICD-10-CM") {
  test_that("compare quirks") {
    expect_true(icd10cmCompare("C74", "C75"));
    expect_true(icd10cmCompare("C74", "C7A"));
    expect_true(icd10cmCompare("C75", "C7A"));
    expect_true(icd10cmCompare("C75", "C7B"));
    expect_true(icd10cmCompare("C7A", "C7B"));
    expect_true(icd10cmCompare("C7B", "C76"));
    expect_false(icd10cmCompare("C76", "C7A"));
    expect_false(icd10cmCompare("C7B", "C7A"));
    expect_false(icd10cmCompare("C7A", "C7A"));
    expect_false(icd10cmCompare("C7B", "C7B"));
    expect_false(icd10cmCompare("C76", "C7B"));
    expect_false(icd10cmCompare("C77", "C76"));
  }
  test_that("compare quirk sub-chap codes") {
    expect_true(icd10cmCompare("C741", "C75"));
    expect_true(icd10cmCompare("C74a", "C7A"));
    expect_true(icd10cmCompare("C752", "C7A"));
    expect_true(icd10cmCompare("C75b", "C7B"));
    expect_true(icd10cmCompare("C7A3", "C7B"));
    expect_true(icd10cmCompare("C7Ac", "C76"));
    expect_false(icd10cmCompare("C7B4", "C7A"));
    expect_false(icd10cmCompare("C7Ad", "C7A"));
    expect_false(icd10cmCompare("C765", "C7A"));
    expect_false(icd10cmCompare("C7Be", "C7B"));
    expect_false(icd10cmCompare("C766", "C7B"));
    expect_false(icd10cmCompare("C77v", "C76"));
    expect_true(icd10cmCompare("C74", "C751"));
    expect_true(icd10cmCompare("C74", "C7A2"));
    expect_true(icd10cmCompare("C75", "C7A3"));
    expect_true(icd10cmCompare("C75", "C7B4"));
    expect_true(icd10cmCompare("C7A", "C7B5"));
    expect_true(icd10cmCompare("C7A", "C766"));
    expect_true(icd10cmCompare("C7B", "C7B0"));
    expect_true(icd10cmCompare("C7A", "C7A8"));
    expect_false(icd10cmCompare("C7B", "C7A7"));
    expect_false(icd10cmCompare("C76", "C7A9"));
    expect_false(icd10cmCompare("C76", "C7Ba"));
    expect_false(icd10cmCompare("C77", "C75b"));
  }
  test_that("compare other quirks") {
    expect_true(icd10cmCompare("Z36", "Z3A"));
    expect_false(icd10cmCompare("Z3A", "Z36"));
    expect_true(icd10cmCompare("Z3A", "Z37"));
    expect_false(icd10cmCompare("Z37", "Z3A"));
    expect_true(icd10cmCompare("M09", "M1A"));
    expect_false(icd10cmCompare("M1A", "M09"));
    expect_true(icd10cmCompare("M1A", "M10"));
    expect_false(icd10cmCompare("M10", "M1A"));
    expect_true(icd10cmCompare("C43", "C4A"));
    expect_false(icd10cmCompare("C4A", "C43"));
    expect_true(icd10cmCompare("C4A", "C44"));
    expect_false(icd10cmCompare("C44", "C4A"));
  }
  test_that("order ICD-10-CM quirks") {
    CharacterVector c = {"Z99", "C76", "C7A", "C7B", "C75", "A00"};
    IntegerVector i   = {6, 5, 3, 4, 2, 1};
    auto o            = icd10cmOrder(c);
    bool res;
    for (auto n = 0; n != c.size() && n != i.size(); ++n) {
      String s = c[n];
      expect_true(o[n] == i[n]);
    }
    c   = {"C7A", "C75", "C76", "C77", "C7B"};
    i   = {2, 1, 4, 5, 3};
    o   = icd10cmOrder(c);
    res = is_true(all(o == i));
    for (auto n = 0; n != c.size() && n != i.size(); ++n) {
      String s = c[n];
      DEBUG_UTIL("o[n] = " << o[n] << ", i[n] = " << i[n]);
      expect_true(o[n] == i[n]);
    }
  }
}
// endif have testthat ICD_CATCH
#endif
