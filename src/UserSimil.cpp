#include <Rcpp.h>
#include <algorithm> 
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix UserSimil(NumericMatrix x, int damp) {
  
  int numItem = x.ncol();
  int numUser = x.nrow();
  
  NumericMatrix sim(numUser, numUser);
  
  double s;//sum of products between user u and v
  double s_u;// sum of squares of scores on item u;
  double s_v;// sum of squares of scores on imte v;
  int c; //co-rated counter
  
  for (int u = 1; u < numUser; u++){
    
    for (int v = 0; v < u; v++){
      
      s = 0;
      s_u = 0;
      s_v = 0;
      c = 0;
      
      for(int i = 0; i < numItem; i++){
        
        if( !R_IsNA( x(u,i) )  &&  !R_IsNA( x(v,i) ) ) {
          
          s += x(u,i) * x(v,i);
          s_u += x(u,i) * x(u,i);
          s_v += x(v,i) * x(v,i);
          c++;
          
        }
        
      }
      
      if((s_u != 0) && (s_v != 0)){
        
        sim(u,v) = (std::max(c,damp) / damp) * s/ sqrt(s_u * s_v);
        //symmetry
        sim(v,u) = sim(u,v);
        
      }
      
    }
    
  }
  
  return sim;
  
}


