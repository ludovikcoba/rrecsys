#include <Rcpp.h>


using namespace Rcpp;

// [[Rcpp::export]]
List FSVDupdaterBiases(NumericMatrix ratingMat, 
                 double learningRate, 
                 double regCoef, 
                 List itemIdx, 
                 int nrfeat, // the total number of features.
                 int steps,
                 double globalbaseline
                 )
  {
  
  NumericMatrix U(ratingMat.nrow(), nrfeat);
  NumericMatrix V(ratingMat.ncol(), nrfeat);

  NumericVector baseline_users(ratingMat.nrow());
  NumericVector baseline_items(ratingMat.ncol());
  

  for(int i = 0; i < ratingMat.nrow(); i++){
    
    baseline_users[i] = R::runif(0,1);
    
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  
  for(int i = 0; i < ratingMat.ncol(); i++){
    
    baseline_items[i] = R::runif(0,1);
    
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  double eij, deltaU, deltaV, pred;
  NumericVector ratingsUser;
  int nr_ratings_user;
  //loop over the users
  for(int ss = 0; ss < steps; ss++){
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
        
        //pred = pred < minimum ? minimum : pred;
        //pred = pred > maximum ? maximum : pred;
        
        eij = ratingMat(i, ratingsUser[j]) - pred;
        
        globalbaseline += learningRate * eij;
        
        baseline_users[i] += learningRate * (eij - regCoef * baseline_users[i]);
        baseline_items[ratingsUser[j]] += learningRate * (eij - regCoef *  baseline_items[ratingsUser[j]]);
        
        for(int feat = 0; feat < nrfeat; feat++){
          // user feature update
          deltaU =  eij * V(ratingsUser[j],feat) - regCoef * U(i,feat);
          //multiply with the learning rate
          deltaU = deltaU * learningRate;
          
          //item feature update
          deltaV = eij * U(i,feat) - regCoef * V(ratingsUser[j],feat);
          //multiply with the learning rate
          deltaV = deltaV * learningRate;
          
          //item & user feature update
          U(i,feat) += deltaU;
          V(ratingsUser[j],feat) += deltaV;
        }
      }
      
    }

  }
  
  List ret;
  ret["U"] = U;
  ret["V"] = V;
  ret["globalAv"] = globalbaseline;
  ret["baseline_users"] = baseline_users;
  ret["baseline_items"] = baseline_items;
  return ret;
  
}

// [[Rcpp::export]]
List FSVDupdater(NumericMatrix ratingMat, 
                       double learningRate, 
                       double regCoef, 
                       List itemIdx, 
                       int nrfeat, // the total number of features.
                       int steps
)
{
  
  double eij, deltaU, deltaV;
  NumericVector ratingsUser;
  int nr_ratings_user;
  
  NumericMatrix U(ratingMat.nrow(), nrfeat);
  NumericMatrix V(ratingMat.ncol(), nrfeat);
  
  
  for(int i = 0; i < ratingMat.nrow(); i++){
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  
  for(int i = 0; i < ratingMat.ncol(); i++){
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  for(int ss = 0; ss< steps; ss++){
    //loop over the users
    for(int i = 0; i < ratingMat.nrow(); i++){
      
      ratingsUser = itemIdx[i];
      //loop over the rated items by a user
      nr_ratings_user = ratingsUser.size();
      
      
      
      for(int j = 0; j < nr_ratings_user; j++){
        
        eij = ratingMat(i, ratingsUser[j]);
        
        // Ui and Vj pair wise multiplication to determine the predicted score.
        for(int k = 0; k < nrfeat; k++){ 
          eij -= U(i,k) * V(ratingsUser[j],k); 
        }
        
        
        
        for(int feat = 0; feat < nrfeat; feat++){
          // user feature update
          deltaU =  eij * V(ratingsUser[j],feat) - regCoef * U(i,feat);
          //multiply with the learning rate
          deltaU = deltaU * learningRate;
          
          //item feature update
          deltaV = eij * U(i,feat) - regCoef * V(ratingsUser[j],feat);
          //multiply with the learning rate
          deltaV = deltaV * learningRate;
          

          //item & user feature update
          U(i,feat) += deltaU;
          V(ratingsUser[j],feat) += deltaV;
        }
        
        
      }
      
    }
  }
  
  
  List ret;
  ret["U"] = U;
  ret["V"] = V;
  return ret;
  
}



