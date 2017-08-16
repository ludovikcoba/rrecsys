#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector predictIB(
    NumericMatrix data,
    NumericMatrix s,
    NumericMatrix simIndexKNN,
    NumericMatrix simil,
    int neigh,
    int maximum,
    int minimum,
    NumericVector ItMeans,
    bool clamp
    ) {

  NumericVector p(s.nrow());
  int u, i, j, count;
  double numer, denom;
  
  for(int l = 0 ; l < s.nrow(); l++){
    
    u = s(l, 0) - 1;
    i = s(l, 1) - 1;
    
    numer = 0;
    denom = 0;
    count = 0;
    
    for(int k = 0; k < neigh; k++){

      j = simIndexKNN(i,k) - 1;
      
      if(!R_IsNA(data(u,j))){
        
        count++;
        denom += fabs(simil(i,j));
        numer += data(u,j) * simil(i,j);
        //numer += data(u,j) * simil(i,j);
        
      }

    }
    
    if(denom == 0 || count < 3){
      p(l) = ItMeans[i];
    }else{
      p(l) = numer/(denom);
    }
    
    //p(l) = ItMeans[i] + numer/(denom + DAMP);
    
    if(clamp){
      
      p(l) = p(l) > maximum ? maximum : p(l);
      p(l) = p(l) < minimum ? minimum : p(l);
      
    }
    
  }
  return p;
}
