#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector predictUB(
    NumericMatrix data,
    NumericMatrix s,
    NumericMatrix simIndexKNN,
    NumericMatrix simil,
    int neigh,
    int maximum,
    int minimum,
    NumericVector UsrMeans,
    bool clamp
) {
  
  NumericVector p(s.nrow());
  int u, i, v, count;
  double numer, denom;
  
  for(int l = 0 ; l < s.nrow(); l++){
    
    u = s(l, 0) - 1;
    i = s(l, 1) - 1;
    
    numer = 0;
    denom = 0;
    count = 0;
    
    for(int k = 0; k < neigh; k++){
      
      v = simIndexKNN(u,k) - 1;
      
      if(!R_IsNA(data(v,i))){
        
        count++;
        denom += fabs(simil(u,v));
        numer += (data(v,i) - UsrMeans[v]) * simil(u,v);
        
      }
      
    }
    
    if(denom == 0 || count < 3){
      p(l) = UsrMeans[u];
    }else{
      p(l) = UsrMeans[u] + numer/denom;
    }
    
    if(clamp){
      
      p(l) = p(l) > maximum ? maximum : p(l);
      p(l) = p(l) < minimum ? minimum : p(l);
      
    }
    
  }
  return p;
}
