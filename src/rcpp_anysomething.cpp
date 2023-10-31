#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.anygreater)]]
bool anygreater(IntegerVector indx, int value) {
  int n = indx.size();
  for(int i = 0; i < n; ++i) {
    if(indx[i] > value) return true;
  }
  return false;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.anynegative)]]
bool anynegative(IntegerVector indx) {
  int n = indx.size();
  for(int i = 0; i < n; ++i) {
    if(indx[i] < 1) return true;
  }
  return false;
}