#include <Rcpp.h>

using namespace Rcpp;

const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
const int BEGINNING = 0;

// [[Rcpp::export]]
NumericMatrix UserSimilSparseMat(
    NumericMatrix x, 
    int dim,
    int damp
) {
  NumericMatrix simil(dim, dim);
  
  int num_ratings = x.nrow();
  int user_u, user_v, i, j;
  double s = 0, s_u = 0, s_v = 0;
  bool go_on = true;
  NumericVector row_pointer(dim+1);
  
  
  user_u = -1;
  int c = 0;
  
  for(int l = 0; l < num_ratings; l++){
    if(x(l,USER) != user_u){
      row_pointer[c] = l;
      c++;
      user_u = x(l,USER);
    }
  }
  
  row_pointer[dim] = - 1;
  
  user_u = 0; //we set u to point to the first user.
  user_v = 1; //we set v to point to the the second user.
  i = row_pointer[user_u];// pointer on the scores of user u;
  j = row_pointer[user_v];// pointer on the scores of user v;
  c = 0;


  while(go_on){

    if(x(i,ITEM) == x(j,ITEM)){
      s += x(i,SCORE) * x(j,SCORE);
      s_u += x(i,SCORE) * x(i,SCORE);
      s_v += x(j,SCORE) * x(j,SCORE);
      i++;
      j++;
      c++;
    }else if(x(i,ITEM) > x(j,ITEM)){
      j++;
    }else if(x(i,ITEM) < x(j,ITEM)){
      i++;
    }
    
    

    if(j == row_pointer[user_v + 1] || i == row_pointer[user_u + 1] || j == num_ratings){
      if((s_u != 0) && (s_v != 0)){
        //
        simil(user_u, user_v) =  (std::max(c,10) /10) * s/sqrt(s_u * s_v);
        simil(user_v, user_u) = simil(user_u, user_v);
        //symmetry
      }

      s = 0;
      s_u = 0;
      s_v = 0;
      c = 0;

      user_u++; //we set u to point to the first user.
      if(user_u == user_v){
        user_u = 0;
        user_v ++;
      }

      i = row_pointer[user_u];// pointer on the scores of user u;
      j = row_pointer[user_v];// pointer on the scores of user v;
      
      if(j == -1) go_on = false;

    }
    
    

  }
  
  return simil;
}
