// use Rcpp11 here if we want: //// [[plugins(cpp11)]]
// Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// include boost
// [[depends(BH)]]

// [[Rcpp::interfaces(r, cpp)]]
// and use them (does this need two compilation steps?)

// TODO: avoid push_back on Rcpp types. The way Rcpp vectors are currently
// implemented this requires copying all of the data each time. This is a very
// expensive operation.

#include <Rcpp.h>
//#include <sstream>
#include <string>
#include <iostream>
#include <boost/algorithm/string/trim.hpp>
#include <icd9.h>

using namespace Rcpp;

// from https://github.com/jjallaire/RcppStrings/blob/master/src/Trim.cpp
namespace {
  // Generic character vector transformation logic
  typedef void (*Transformer)(std::string& str, const std::locale&);
  CharacterVector stringTransform(CharacterVector input, Transformer transform) {
    const std::locale& locale = std::locale();
    CharacterVector output(input.size());
    for (int i = 0; i<input.size(); i++) {
      std::string str(input[i]);
      transform(str, locale);
      output[i] = str;
    }
    return output;
  }
} // anonymous namespace

//' @title Trim whitespace from start and/or end of string
//' @usage trim(input, side = "both")
//' @param input input character vector
//' @param side side on which whitespace is removed (left, right, or both)
//' @return character vector with leading and trailing whitespace removed
//' @examples
//' \dontrun{
//' trimString("\n\nString with trailing and leading white space \t")
//' }
// [[Rcpp::export("stringTrim")]]
CharacterVector stringTrim(CharacterVector input, std::string side = "both") {
  // select correct transformation
  Transformer transform = NULL;
  if (side == "both")
  transform = boost::algorithm::trim<std::string>;
  else if (side == "left")
  transform = boost::algorithm::trim_left<std::string>;
  else if (side == "right")
  transform = boost::algorithm::trim_right<std::string>;
  else {
    stop("Unrecognized side argument '" + side + "'");
    return input; // keep compiler happy
  }
  // perform the transformation
  return stringTransform(input, transform);
}


std::vector< std::string > concatsv(std::vector< std::string > x,
std::vector< std::string > y) {
  std::vector< std::string > v;
  v.reserve(x.size() + y.size());
  v.insert(v.end(), x.begin(), x.end());
  v.insert(v.end(), y.begin(), y.end());
  return v;
}

CharacterVector concatcv(CharacterVector cvx,
CharacterVector cvy) {
  std::vector< std::string > v;
  std::vector< std::string > x = as<std::vector< std::string > >(cvx);
  std::vector< std::string > y = as<std::vector< std::string > >(cvy);
  v = concatsv(x,y); // TODO: must i copy?
  return wrap(v);
}

