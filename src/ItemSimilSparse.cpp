#include <Rcpp.h>


using namespace Rcpp;
const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
const int BEGINNING = 0;


// [[Rcpp::export]]
NumericMatrix ItemSimilSparseMat(
    NumericMatrix x, 
    int dim,
    int DAMP
) {
  NumericMatrix simil(dim, dim);
  
  
  long num_ratings = x.nrow();
  long item_u, item_v, i, j;
  double s = 0, s_u = 0, s_v = 0;
  bool go_on = true;
  NumericVector col_pointer(dim+1);
  
  
  item_u = -1;
  int c = 0;
  
  for(int l = 0; l < dim; l++) col_pointer[l] = -2; //missing item
  
  for(int l = 0; l < num_ratings; l++){
    if(x(l,ITEM) != item_u){
      item_u = x(l,ITEM);
      col_pointer[item_u - 1] = l;
    }
  }
  
  col_pointer[dim] = num_ratings;//finish
  
  
  item_u = 0; //we set u to point to the first item.
  item_v = 1; //we set v to point to the the second item.
  i = col_pointer[item_u];// pointer on the scores of item u;
  j = col_pointer[item_v];// pointer on the scores of item v;
  c = 0;
  
  
  while(go_on){
    
    if(x(i,USER) == x(j,USER)){
      s += x(i,SCORE) * x(j,SCORE);
      s_u += x(i,SCORE) * x(i,SCORE);
      s_v += x(j,SCORE) * x(j,SCORE);
      i++;
      j++;
      c++;
    }else if(x(i,USER) > x(j,USER)){
      j++;
    }else if(x(i,USER) < x(j,USER)){
      i++;
    }

    
    if(j >= col_pointer[item_v + 1] || i >= col_pointer[item_u + 1] ){
      
      if((s_u != 0) && (s_v != 0)){
        
        simil(item_u, item_v) = (std::max(c,DAMP) / DAMP) * s/sqrt(s_u * s_v);
        simil(item_v, item_u) = simil(item_u, item_v);
        //symmetry
      }
      
      s = 0;
      s_u = 0;
      s_v = 0;
      c = 0;
      
      item_u++;
      
      if(item_u >= item_v){
        item_u = 0; //we set u to point to the first user.
        item_v ++;
      }
      
      i = col_pointer[item_u];// pointer on the scores of user u;
      j = col_pointer[item_v];// pointer on the scores of user v;
      
      
    }
    
    if(j >= num_ratings) {
      go_on = false;
      
    }
    
  }
  
  return simil;
}
