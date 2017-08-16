#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector itemMFCount(
  NumericMatrix sim_index, 
  NumericVector user_vector,
  double pt
) {
  
  NumericVector counter(user_vector.size());
  
  int similar_item;
  
  for(int i = 0; i < user_vector.size(); i++){
    if(R_IsNA(user_vector(i))){
      for(int j = 0; j < sim_index.ncol(); j++){
        similar_item = sim_index(i,j) - 1;
        if(user_vector(similar_item) >=  pt) counter(i)++;
      }
    }
  }
  
  
  
  return counter;
}
