#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
StringVector create_cid(StringVector tipdoc, StringVector nrodoc) {
  std::vector<std::string> tipdoc1 = as<std::vector<std::string>>(tipdoc);
  std::vector<std::string> nrodoc1 = as<std::vector<std::string>>(nrodoc);
  if (tipdoc1.size() == nrodoc1.size()) {
    std::vector<std::string> out(nrodoc1.size());
    for (int i = 0; i < tipdoc1.size(); i++) {
      if (tipdoc1[i] == "DNI") {
        tipdoc1[i] = "D";
      } else if (tipdoc1[i] == "RUC") {
        tipdoc1[i] = "R";
      } else if (tipdoc1[i] == "CE") {
        tipdoc1[i] = "E";
      } else if (tipdoc1[i] == "PAS") {
        tipdoc1[i] = "P";
      } else if (tipdoc1[i] == "OTR") {
        tipdoc1[i] = "O";
      }
      out[i] = tipdoc1[i] + '-' + std::string(14 - nrodoc1[i].length(), '0') + nrodoc1[i];
    }
    return tipdoc;
  }
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// /*** R
// create_cid(c('DNI','PAS'), c('70506615','123456'))
// */
