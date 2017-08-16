#include <Rcpp.h>
using namespace Rcpp;



const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
const int BEGINNING = 0;

// [[Rcpp::export]]
NumericVector predictUBSparse(
  NumericMatrix data,
  int dim,
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
  NumericVector row_pointer(dim+1);
  int u, i, v, count;
  double numer, denom;
  bool same_user = true;
  
  //possible error due to missing users
  count = 0;
  int user_u = -1;
  for(int l = 0; l < data.nrow(); l++){
    if(data(l,USER) != user_u){
      row_pointer[count] = l;
      count++;
      user_u = data(l,USER);
    }
  }
  
  row_pointer[dim] = dim;
  int j;
  
  for(int l = 0 ; l < s.nrow(); l++){
    
    u = s(l, 0) - 1;
    i = s(l, 1);
    
    numer = 0;
    denom = 0;
    count = 0;
    
    for(int k = 0; k < neigh; k++){
      
      v = simIndexKNN(u,k) - 1;
      j = row_pointer[v];
      same_user = true;
      
      while(same_user){
        if(i == data(v,2)){
          count++;
          denom += fabs(simil(u,v-1));
          numer += (data(j,2) - UsrMeans[v-1]) * simil(u,v-1); 
        }
        
        j++;
        
        if(j == row_pointer[v + 1]){
          same_user = false;
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
  }

  return p;
}
