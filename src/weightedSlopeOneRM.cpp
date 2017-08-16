#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List weightedSlopeOneRM(NumericMatrix x) {

  
  int nc = x.ncol();
  int nr = x.nrow();
  int coRated;
  NumericMatrix Deviation(nc,nc);
  NumericMatrix Cardinality(nc,nc);
  double dev;
  
  for(int i = 1; i < nc; i++){
    for(int j = 0; j < i; j++){
      
      coRated = 0;
      dev = 0;
      
      for(int k = 0; k < nr; k++){
        
        if(!R_IsNA(x(k,i)) && !R_IsNA(x(k,j))){
          
          coRated++;
          dev += x(k,i) - x(k,j);
          
        }
        
      }
      
      if(coRated != 0) dev = dev/coRated;
      
      Deviation(i,j) = dev;
      Deviation(j,i) = -dev;
      Cardinality(i,j) = coRated;
      Cardinality(j,i) = coRated;
      
    }
  }
  
  List res;
  res["Deviation"] = Deviation;
  res["Cardinality"] = Cardinality;
  
  return res;
}

