#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

List FSVDupdater(NumericMatrix ratingMat, 
                 NumericMatrix U, 
                 NumericMatrix V, 
                 double gamma, 
                 double lambda, 
                 List itemIdx, 
                 int nrfeat, // the total number of features.
                 int feat,//the feature that we are currently updating!
                 double globalbaseline,
                 NumericVector baseline_users,
                 NumericVector baseline_items,
                 double minimum, //domain minimum.
                 double maximum  //domain maximal value.
                 )
  {
  int eij;
  double pred;
  NumericVector ratingsUser;
  int nr_ratings_user;
  //loop over the users
  for(int i = 0; i < ratingMat.nrow(); i++){
    ratingsUser = itemIdx[i];
    //loop over the rated items by a user
    nr_ratings_user = ratingsUser.size();
    
    for(int j = 0; j < nr_ratings_user; j++){
      
      // i subtract 1 to ratingsUser[j] since indices on vector in R start from 1 insead in they start from 0 in cpp. 
      pred = baseline_users[i] + baseline_items[(ratingsUser[j])];
      
      // Ui and Vj pair wise multiplication to determine the predicted score.
      for(int n = 0; n < nrfeat; n++){ 
        pred += U(i,n) * V(ratingsUser[j],n); 
        }
      
      // clamping prediction to domain.
      //if(pred < minimum) pred = minimum;
      //if(pred > maximum) pred = maximum;
      
      eij = ratingMat(i, ratingsUser[j]) - pred;
      // user feature update
      U(i,feat) += lambda * (eij * V(ratingsUser[j],feat) - gamma * U(i,feat));
      //item feature update
      V(ratingsUser[j],feat) += lambda * (eij * U(i,feat) - gamma * V(ratingsUser[j],feat));
      
    }
    
  }
  List ret;
  ret["U"] = U;
  ret["V"] = V;
  return ret;
  
}