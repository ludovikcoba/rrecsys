#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector predictUVBiases(
    NumericMatrix s,
    NumericMatrix U,
    NumericMatrix V,
    int minimum,
    int maximum,
    double blglo,
    NumericVector blusr,
    NumericVector blitm,
    bool clamp
){
  
  NumericVector p(s.nrow());
  
  for(int xx = 0; xx < s.nrow(); xx++) p(xx) = 0;
  
  int i, j;
  int f = U.ncol();
  
  for(int l = 0; l < s.nrow(); l ++){
    i = s(l, 0) - 1;
    j = s(l, 1) - 1;
    
    for(int k = 0; k < f; k++) p(l) += U(i,k) * V(j,k);
    
    p(l) += blglo + blusr(i) + blitm(j);
    
    if(clamp){
      p(l) = p(l) > maximum ? maximum : p(l);
      p(l) = p(l) < minimum ? minimum : p(l);
    }

    
  }
  return p;
}

// [[Rcpp::export]]
NumericVector predictUV(
    NumericMatrix s,
    NumericMatrix U,
    NumericMatrix V,
    int minimum,
    int maximum,
    bool clamp
){
  NumericVector p(s.nrow());
  
  for(int xx = 0; xx < s.nrow(); xx++) p(xx) = 0;
  
  int i, j;
  int f = U.ncol();
  
  for(int l = 0; l < s.nrow(); l ++){
    i = s(l, 0) - 1;
    j = s(l, 1) - 1;
    
    for(int k = 0; k < f; k++) p(l) += U(i,k) * V(j,k);
    
    if(clamp){
      p(l) = p(l) > maximum ? maximum : p(l);
      p(l) = p(l) < minimum ? minimum : p(l);
    }
    
  }
  return p;
}
