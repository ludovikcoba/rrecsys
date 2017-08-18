# k is the number of features.
# learningRate is the learning rate. 
# regCoef is the regularization term.

# Reference: Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for Recommender Systems.
#            S. Funk. Netflix Update: Try this at Home.
setMethod(performSVD, signature = c("dataSet"), function(data, 
                                                         k = 10, 
                                                         learningRate = 0.001, 
                                                         regCoef = 0.001,
                                                         biases = FALSE,
                                                         steps = 10){
  x <- data@data

  row_x <- nrow(data)
  col_x <- ncol(data)
  if (col_x  < k | row_x < k) 
    stop("Invalid number of features! \nLess features than the actual number of items or users! Please correct k!")
  # determine the means over row and columns and the total mean

  ptm <- Sys.time()
  
  # initilize the user and item features

  #list of indices pointing to rated items. Remove 1 to pass it as a c++ index. 
  itemIDX <- lapply(1:row_x, 
                    function(temp) {
                      
                      (which(!is.na(x[temp, ]))) - 1
                      
                    })


  if(biases){

    res <- FSVDupdaterBiases( 
      x, 
      learningRate, 
      regCoef, 
      itemIDX, 
      k, 
      steps,
      averageRating(data)
    )
    
    
    b_line <- list(globalAv = res$globalAv, baseline_users = res$baseline_users, baseline_items =  res$baseline_items)
    
  }else{
    
    b_line <- list(NA)
    
    
    res <- FSVDupdater( 
      x, 
      learningRate, 
      regCoef, 
      itemIDX, 
      k, 
      steps
    )
    
  }
  
  U <- res$U
  V <- res$V
  
  cat("Features trained. Time:", as.numeric(Sys.time() - ptm, units = "secs"), "seconds. \n")
  
  p_FunkSVD <- list(k = k, 
                    learningRate = learningRate, 
                    regCoef = regCoef,
                    biases = biases)
  
  new("SVDclass", 
      alg = "FunkSVD", 
      data = data, 
      factors = list(U = U, V = V), 
      parameters = p_FunkSVD, 
      baselines = b_line
  )
  
})



#SparseMat SVD####
setMethod("performSVD", signature = c("sparseDataSet"), function(data, 
                                                                 k, 
                                                                 learningRate, 
                                                                 regCoef,
                                                                 biases,
                                                                 steps
                                                                 ){
  ptm <- Sys.time()

  if(biases){
    b_line <- baseline(data)
  }else{
    b_line <- list(NA)
  }

  
  # pair-whise multiplication on the first round is the sum intiValue^2, k - times  

  # the training feature loop

  if(biases){
  
    res <- FSVDupdaterSparseMatBiases( 
      as.matrix(data@data),
      learningRate, 
      regCoef, 
      k,
      steps,
      nrow(data),
      ncol(data),
      averageRating(data)
    )
    
    b_line <- list(globalAv = res$globalAv, baseline_users = res$baseline_users, baseline_items =  res$baseline_items)
    
  }else{
    
    b_line <- list(NA)

    res <- FSVDupdaterSparseMat( 
      as.matrix(data@data),
      learningRate, 
      regCoef, 
      k,
      steps,
      nrow(data),
      ncol(data)
    )
      
  }  #end feature loop

  U <- res$U
  V <- res$V
  
  p_FunkSVD <- list(k = k, 
                    learningRate = learningRate, 
                    regCoef = regCoef,
                    biases = biases)
  
  new("SVDclass", 
      alg = "FunkSVD", 
      data = data, 
      factors = list(U = U, V = V), 
      parameters = p_FunkSVD, 
      baselines = b_line
  )
  
  
})


#main call to the method####
FunkSVD <- function(data, 
                    k = 10, 
                    learningRate = 0.001, 
                    regCoef = 0.001,
                    biases = FALSE,
                    steps = 10) 
  {
  
  performSVD(data, 
             k, 
             learningRate, 
             regCoef,
             biases,
             steps)
  
}

p_FunkSVD <- list(
  
  k = 10, 
  learningRate = 0.001, 
  regCoef = 0.015,
  biases = FALSE
  
  )

#registry entry####
rrecsysRegistry$set_entry(alg = "FunkSVD", 
                          fun = FunkSVD, 
                          description = "Funk's SVD", 
                          reference =  "Y. Koren, R. Bell, and C. Volinsky. Matrix Factorization Techniques for Recommender Systems. \nS. Funk. Netflix Update: Try this at Home.",
                          parameters = p_FunkSVD) 

#prediction method####
setMethod("predict", signature = c(model = "SVDclass"), function(model, Round = FALSE, s, clamp = TRUE) {

  
  if(missing(s)){
    item_not_rated <- which(is.na(model@data@data))
    
    # generate predictions
    p <- model@factors$U %*% t(model@factors$V) 
    

    if(model@parameters$biases){
      
      p <- p + model@baselines$globalAv
      
      #add line and row baseline
      for(i in 1:nrow(model@data)) p[i, ] <- p[i,] + model@baselines$baseline_users[i]
      
      for(j in 1:ncol(model@data)) p[ , j] <- p[, j] + model@baselines$baseline_items[j]
    
    }
    
    p[which(p < model@data@minimum)] <- model@data@minimum
    
    p[which(p > model@data@maximum)] <- model@data@maximum
    
    model@data@data[item_not_rated] <- p[item_not_rated]
    
    return(p)

    
  }else{
    
    minimum <- model@data@minimum
    maximum <- model@data@maximum

    if(model@parameters$biases){
    p <- predictUVBiases(
      as.matrix(s), 
      model@factors$U,
      model@factors$V,
      minimum,
      maximum,
      model@baselines$globalAv,
      model@baselines$baseline_users,
      model@baselines$baseline_items,
      clamp
      )
    }else{
    p <- predictUV(
      as.matrix(s), 
      model@factors$U,
      model@factors$V,
      minimum,
      maximum,
      clamp
      )
    }
    


    return(p)
  }

})









