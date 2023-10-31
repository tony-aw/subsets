#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sb_str)]]
std::string rcpp_sb_str(std::string x, IntegerVector ind) {
  std::string out = "";
  int n = ind.size();
  for(int i = 0; i < n; ++i) {
    out += x.at(ind[i]);
  }
  return out;
}


