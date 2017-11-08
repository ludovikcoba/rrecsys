#include <Rcpp.h>
using namespace Rcpp;
const int USER = 0;
const int ITEM = 1;
const int SCORE = 2;

// [[Rcpp::export]]

List FSVDupdaterSparseMatBiases(
    NumericMatrix sparseRatingMat,
    double learningRate, 
    double regCoef,
    int nrfeat, // the total number of features.
    int steps,
    int nr_users,
    int nr_items,
    double globalbaseline
  )
{
  
  NumericMatrix U(nr_users, nrfeat);
  NumericMatrix V(nr_items, nrfeat);
  
  NumericVector baseline_users(nr_users);
  NumericVector baseline_items(nr_items);
  
  
  for(int i = 0; i < nr_users; i++){
    
    baseline_users[i] = R::runif(0,1);
    
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  for(int i = 0; i <  nr_items; i++){
    
    baseline_items[i] = R::runif(0,1);
    
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }

  double eij, deltaUif, deltaVjf, pred;

  int i, j;
    
    for(int ss = 0; ss<steps; ss++){
      for(int k = 0; k < sparseRatingMat.nrow(); k++){
        
        // -1 to change the index according to C++.
        //user index.
        i = sparseRatingMat(k,USER) - 1;
        //item index.
        j = sparseRatingMat(k,ITEM) - 1;
        
        //add baseline to prediction.
        pred = globalbaseline + baseline_users[i] + baseline_items[j];
        
        //compute paiwise multiplication on the features.
        for(int l = 0; l < nrfeat; l++){
          pred += U(i,l) * V(j,l);
        }
        
        //compute error
        eij = sparseRatingMat(k,SCORE) - pred;
        
        globalbaseline += learningRate * eij;
        
        
        baseline_users[i] += learningRate * (eij - regCoef * baseline_users[i]);
        baseline_items[j] += learningRate * (eij - regCoef * baseline_items[j]);
        
        
        for(int feat = 0; feat < nrfeat; feat++){
          //item feature 
          deltaVjf = learningRate * (eij * U(i,feat) -regCoef * V(j,feat));
          
          // user feature 
          deltaUif = learningRate * (eij * V(j,feat) - regCoef * U(i,feat));
          
          // update prediction.
          //p[k] += deltaUif * V(j,feat) + U(i,feat) * deltaVjf +  deltaUif * deltaVjf;
          
          //update
          V(j,feat) += deltaVjf;
          U(i,feat) += deltaUif;
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
List FSVDupdaterSparseMat(
    NumericMatrix sparseRatingMat,
    double learningRate, 
    double regCoef,
    int nrfeat, // the total number of features.
    int steps,
    int nr_users,
    int nr_items
  )
{
  
  NumericMatrix U(nr_users, nrfeat);
  NumericMatrix V(nr_items, nrfeat);
  
  
  for(int i = 0; i < nr_users; i++){
    for(int j = 0; j < nrfeat ; j++){
      U(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }
  
  for(int i = 0; i <  nr_items; i++){
    for(int j = 0; j < nrfeat ; j++){
      V(i,j) = R::runif(0,1) * sqrt(0.5f/nrfeat);
    }
  }

  double eij, deltaUif, deltaVjf, pred;

  int i, j;
    
    for(int ss = 0; ss < steps; ss++){
      
      for(int k = 0; k < sparseRatingMat.nrow(); k++){
        
        pred = 0;
        // -1 to change the index according to C++.
        //user index.
        i = sparseRatingMat(k,USER) - 1;
        //item index.
        j = sparseRatingMat(k,ITEM) - 1;
        
        //compute paiwise multiplication on the features.
        for(int l = 0; l < nrfeat; l++){
          pred += U(i,l) * V(j,l);
        }
        
        //compute error
        eij = sparseRatingMat(k,SCORE) - pred;
        
        for(int feat = 0; feat < nrfeat; feat++){
          //item feature 
          deltaVjf = learningRate * (eij * U(i,feat) - regCoef * V(j,feat));
          
          // user feature 
          deltaUif = learningRate * (eij * V(j,feat) - regCoef * U(i,feat));
          
          //update
          V(j,feat) += deltaVjf;
          U(i,feat) += deltaUif;
        }
        
      }
    }
    
  
    
    
  List ret;
  ret["U"] = U;
  ret["V"] = V;

  return ret;
  
}

