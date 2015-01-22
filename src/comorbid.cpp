#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

typedef std::vector<std::string > VecStr;
typedef std::set<std::string > SetStr;
typedef std::vector<SetStr > VecSetStr;

//' @rdname icd9Comorbid
//' @export
// [[Rcpp::export]]
List icd9ComorbidShort(
  DataFrame icd9df,
  List icd9Mapping,
  std::string visitId = "visitId", // or CharacterVector?
  std::string icd9Field = "icd9"
  ) {
    //CharacterVector uv = unique(vso);
    VecStr vs = as<VecStr>(as<CharacterVector>(icd9df[visitId]));
    VecStr icds = as<VecStr>(as<CharacterVector>(icd9df[icd9Field])); //
    SetStr uniqvs(vs.begin(), vs.end());
    int usize = uniqvs.size();

    // convert mapping from List of CharacterVectors to std vector of sets. This
    // is a small one-off cost, and dramatically improves the performance of the
    // later loops.
    VecSetStr map;
    for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
      VecStr mvs(as<VecStr>(*mi));
      SetStr ss(mvs.begin(), mvs.end());
      map.push_back(ss);
    }

    int nref = map.size();
    LogicalMatrix out(usize, nref); // fills with FALSE

    std::string lastv("");
    // loop through rows or cols first. may be easier to do the rows first, so
    // we can aggregate as we go along
    int nrow = vs.size();
    int outrow = 0;
    for (int vr = 0; vr < nrow; ++vr) {
      //std::cout << "vr = " << vr << "\n";
      std::string icd = icds[vr];
      std::string v = vs[vr];

      //std::cout << "outrow = " << outrow << "\n";
      // assume that unique visitIds are grouped together... we could sort first
      // to make sure of it, which is probably not a huge cost.
      if (v.compare(lastv) != 0) {
        // new visit, so start a new row in the output matrix.
        outrow += 1;
      }
      for (int cmb = 0; cmb < nref; ++cmb) {
        //std::cout << "cmb = " << cmb << "\n";
        if (map[cmb].find(icd) != map[cmb].end()) {
          out(outrow, cmb) = true;
        }
      }
    }

    // this look clumsy, but suggested by a rcpp developer:
    // http://stackoverflow.com/questions/24352208/best-way-to-convert-dataframe-to-matrix-in-rcpp
    // DataFrame outdf = internal::convert_using_rfunction(x, "as.data.frame");

    // but I need a visitId column (ideally in the first position), so let's do it manually:
    List outdf;
    outdf[visitId] = uniqvs;
    CharacterVector cn = icd9Mapping.names();
    for (int ci = 0; ci < cn.size(); ++ci) {
      // write each column
      LogicalVector lv = out( _, ci);
      String nm = cn[ci]; // get name of current comorbidity
      outdf[nm] = lv;
    }

    IntegerVector row_names = seq_len(usize);
    outdf.attr("row.names") = row_names;
    outdf.attr("class") = "data.frame";

    return outdf;
  }
