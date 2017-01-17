# k is the number of features.
# gamma is the regularization term. 
# lambda is the learning rate.

# Reference: Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for Recommender Systems.
#            S. Funk. Netflix Update: Try this at Home.

FunkSVDcpp <- function(data, k = 10, gamma = 0.015, lambda = 0.001) {
  
  x <- data@data
  
  colnames(x) <- NULL
  rownames(x) <- NULL
  
  row_x <- nrow(x)
  col_x <- ncol(x)
  if (col_x  < k | row_x < k) 
    stop("Invalid number of features! \nLess features than the actual number of items or users! Please correct k!")
  # determine the means over row and columns and the total mean
  #biases <- calcBias(x)
  # initilize the user and item features
  U <- matrix(0.1, nrow = row_x, ncol = k)
  V <- matrix(0.1, nrow = col_x, ncol = k)

  #list of indices pointing to rated items. Remove 1 to pass it as a c++ index. 
  itemIDX <- lapply(1:row_x, 
                    function(temp) {
                      
                      (which(!is.na(x[temp, ]))) - 1
                      
                    })
  
  b_line <- baseline(data)
  
  # the training feature loop
  for (f in 1:k) {
    #restart counter and error for new feature
    resetrrecsysenv()
    
    ptm <- Sys.time()
    
    while (!isConverged(x, U, V)) {
      
      res <- FSVDupdater( 
        x, 
        U, 
        V, 
        gamma, 
        lambda, 
        itemIDX, 
        k, 
        f-1,#indices in c++ start from 0 instead of 1.
        b_line$globalAv,
        b_line$baseline_users,
        b_line$baseline_items,
        data@minimum,
        data@maximum
      )
      
      U <- res$U
      V <- res$V
      
    }  #end convergence loop
    
    cat("Feature:", f, "/", k, " trained. Time:", as.numeric(Sys.time() - ptm, units = "secs"), "seconds. \n")
    
  }  #end feature loop
  
  p_FunkSVD <- list(k = k, 
                    gamma = gamma, 
                    lambda = lambda)
  
  new("SVDclass", 
      alg = "FunkSVD", 
      data = data, 
      factors = list(U = U, V = V), 
      parameters = p_FunkSVD, 
      baselines = b_line
  )
  
}


p_FunkSVD <- list(k = 10, gamma = 0.01, lambda = 0.001)
rrecsysRegistry$set_entry(alg = "FunkSVD", 
                          fun = FunkSVDcpp, 
                          description = "Funk's SVD", 
                          reference =  "Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for Recommender Systems. \nS. Funk. Netflix Update: Try this at Home.",
                          parameters = p_FunkSVD) 



# predict####
setMethod("predict", signature = c(model = "SVDclass"), function(model, Round = FALSE) {
  
  
  item_not_rated <- which(is.na(model@data@data))
  
  # generate predictions
  p <- model@factors$U %*% t(model@factors$V) + model@baselines$globalAv
  
  #add line and row baseline
  for(i in 1:nrow(model@data)) p[i, ] <- p[i,] + model@baselines$baseline_users[i]
  
  for(j in 1:ncol(model@data)) p[ , j] <- p[, j] + model@baselines$baseline_items[j]
  
  p[which(p < model@data@minimum)] <- model@data@minimum
  
  p[which(p > model@data@maximum)] <- model@data@maximum
  
  model@data@data[item_not_rated] <- p[item_not_rated]
  
  return(roundData(model@data, Round))
  
})
