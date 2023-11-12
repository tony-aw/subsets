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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sb_str_rp)]]
std::string rcpp_sb_str_rp(std::string x, IntegerVector ind, std::string rp_x, IntegerVector rp_ind) {
  int n = ind.size();
  for(int i = 0; i < n; ++i) {
    x.at(ind[i]) = rp_x.at(rp_ind[i]);
  }
  return x;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_str_ind_is_nonpos)]]
bool rcpp_str_ind_is_nonpos(IntegerVector indx1, IntegerVector indx2) {
  R_xlen_t n = indx1.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(indx1[i] < 1 || indx2[i] < 1) return true;
  }
  return false;
}