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
  
  double eij, deltaU, deltaV, pred;
  NumericVector ratingsUser;
  int nr_ratings_user;
  //loop over the users
  for(int i = 0; i < ratingMat.nrow(); i++){
    
    ratingsUser = itemIdx[i];
    //loop over the rated items by a user
    nr_ratings_user = ratingsUser.size();
    
    for(int j = 0; j < nr_ratings_user; j++){
      // TO DO: the prediction loop may be fixed to perform better!!!!
      pred = globalbaseline + baseline_users[i] + baseline_items[ratingsUser[j]];
      
      // Ui and Vj pair wise multiplication to determine the predicted score.
      for(int k = 0; k < nrfeat; k++){ 
        pred += U(i,k) * V(ratingsUser[j],k); 
      }
      
      eij = ratingMat(i, ratingsUser[j]) - pred;
      
      // user feature update
      deltaU =  eij * V(ratingsUser[j],feat) - lambda * U(i,feat);
      //multiply with the learning rate
      deltaU = deltaU * gamma;
      
      //item feature update
      deltaV = eij * U(i,feat) - lambda * V(ratingsUser[j],feat);
      //multiply with the learning rate
      deltaV = deltaV * gamma;
      
      //item & user feature update
      U(i,feat) += deltaU;
      V(ratingsUser[j],feat) += deltaV;
    }
    
  }
  List ret;
  ret["U"] = U;
  ret["V"] = V;
  return ret;
  
}