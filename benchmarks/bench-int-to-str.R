//' @examples
//' \dontrun{
//' pts <- generate_random_pts(1e7)
//' # conclusion: buffer size matters little (so default to be more generous),
//' # and 'Rcpp' version fastest.
//' microbenchmark::microbenchmark(fastIntToStringStd(pts$visit_id, buffer = 8),
//'                                fastIntToStringStd(pts$visit_id, buffer = 16),
//'                                fastIntToStringStd(pts$visit_id, buffer = 64),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 8),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 16),
//'                                fastIntToStringRcpp(pts$visit_id, buffer = 64),
//'                                as.character(pts$visit_id),
//'                                as_char_no_warn(pts$visit_id), times = 5)
//' }

