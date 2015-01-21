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

//' @title find comorbidities from ICD-9 codes.
//' @description This is the main function which extracts co-morbidities from a
//'   set of ICD-9 codes. This is when some trivial post-processing of the
//'   comorbidity data is done, e.g. renaming to human-friendly field names, and
//'   updating fields according to rules. The exact fields from the original
//'   mappings can be obtained using \code{applyHierarchy = FALSE}, but for
//'   comorbidity counting, Charlson Score, etc., the rules should be applied.
//' @template icd9df
//' @param icd9Mapping list (or name of a list if character vector of length one
//'   is given as argument) of the comorbidities with each top-level list item
//'   containing a vector of decimal ICD9 codes. This is in the form of a list,
//'   with the names of the items corresponding to the comorbidities (e.g. "HTN",
//'   or "diabetes") and the contents of each list item being a character vector
//'   of short-form (no decimal place but ideally zero left-padded) ICD-9 codes.
//'   No default: user should prefer to use the derivative functions, e.g.
//'   icd9ComorbidAhrq, since these also provide appropriate naming for the
//'   fields, and squashing the hierarchy (see \code{applyHierarchy} below)
//' @template visitid
//' @template icd9field
//' @details There is a change in behavior from previous versions. The visitId
//'   column is (implicitly) sorted by using std::set container. Previously, the
//'   visitId output order was whatever R's \code{aggregate} produced.
//' @examples
//'   pts = data.frame(visitId = c("2", "1", "2", "3", "3"),
//'                    icd9 = c("39891", "40110", "09322", "41514", "39891"))
//'    icd9ComorbidShort(pts, ahrqComorbid) # visitId is now sorted
//' @export
// [[Rcpp::export]]
DataFrame icd9ComorbidShort(
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
    int nref = icd9Mapping.size();
    LogicalMatrix out(usize, nref); // fills with FALSE

    // convert mapping from List of CharacterVectors to std vector of sets. This
    // is a small one-off cost, and dramatically improves the performance of the
    // later loops.
    VecSetStr map;
    for (List::iterator mi = icd9Mapping.begin(); mi != icd9Mapping.end(); ++mi) {
      VecStr mvs(as<VecStr>(*mi));
      SetStr ss(mvs.begin(), mvs.end());
      map.push_back(ss);
    }

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
      String nm = cn[ci];
      outdf[nm] = lv;
    }

    IntegerVector row_names = seq_len(usize);
    outdf.attr("row.names") = row_names;
    outdf.attr("class") = "data.frame";

    return outdf;
  }
