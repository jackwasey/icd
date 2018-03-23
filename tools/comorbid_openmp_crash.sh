#!/bin/sh

# discovered that Rcpp::Rcout is not thread-safe, so crashed when debug code was defined. No crash without Rcpp::Rcout.

R --quiet --vanilla -e 'pts <- icd:::generate_random_pts(5e5); icd::icd_comorbid(pts, icd::icd9_map_ahrq, comorbid_fun = icd:::icd9ComorbidShortCpp) -> devnull'

