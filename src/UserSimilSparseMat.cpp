#include <Rcpp.h>

using namespace Rcpp;

const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;
//const int BEGINNING = 0;

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
  int c = 0;
  
  i=0;
  user_u = x(i, USER); //we set u to point to the first user.
  
  j=0;
  while(x(j, USER) == user_u) j++;
  user_v = x(j, USER); //we set v to point to the the second user.



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

    
    if(x(j,USER) != user_v || x(i,USER) != user_u || j == num_ratings){
      if((s_u != 0) && (s_v != 0)){
        //
        simil(user_u, user_v) =  (std::max(c,damp) /damp) * s/sqrt(s_u * s_v);
        simil(user_v, user_u) = simil(user_u, user_v);
        //symmetry
      }

      s = 0;
      s_u = 0;
      s_v = 0;
      c = 0;

      user_u++; //we set u to point to the first user.
      if(user_u == user_v){
        user_u = x(0, USER); //reset pointer to user 1
        while(j != num_ratings || x(j, USER) == user_v) j++;//go to next user
        
        if(j == num_ratings) {go_on = false;}
        else {user_v = x(j, USER);}
        
      }

    }
    
    if(j == num_ratings) go_on = false;

  }
  
  return simil;
}
