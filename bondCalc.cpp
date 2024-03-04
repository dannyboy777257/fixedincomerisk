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
double bondPrice(double ytm, int faceValue, double coupon, double ttm, int freq) {
  
  double couponPayment = (coupon * faceValue) / freq;
  double bondPrice = 0.0;
  int totalPeriods = ceil(ttm * freq);
  
  for (int t = 1; t <= totalPeriods; t++) {
    
    bondPrice += couponPayment / pow(1 + ytm / freq, t);
    
  }
  
  bondPrice += faceValue / pow(1 +  ytm / freq, totalPeriods);
  
  return bondPrice;
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

///*** R
//*/
