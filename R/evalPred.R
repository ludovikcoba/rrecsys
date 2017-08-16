setMethod("evalPred", signature = c(model = "evalModel"), function(model, alg = NULL, ...) {
    
    if (is.null(alg)) {
      
        stop("Evaluation on recommendations cannot proceed if argument alg and is not specified.")
      
    }
  d <- model@data
  nusers <- nrow(d)
  folds <- model@folds
  
  #user based RMSE & MAE
  uRMSE <- c()
  uMAE <- c()
  #global RMSE & MAE
  gRMSE <- c()
  gMAE <- c()
  
  ex.time <- c()
  
  for (i in 1:folds) {
    
    ptm <- Sys.time()
    
    testSetIDX <- model@fold_indices[[i]]
    
    x <- removeScores(d, testSetIDX)
    
    cat("Train set", 
        i, "/", folds, 
        "created with ", 
        nrow(x), "users, ", 
        ncol(x),"items and ", 
        numRatings(x), " ratings.\n")
    
    r <- rrecsys(x, alg = alg, ...)
    

    # mae & rmse on user
    # s is a pair of the coordinates of all the user item ratings in the test set.
    
    users_rmse <- 0
    users_mae <- 0
    
    if(class(d) == "sparseDataSet"){
      
      s <- d@data[testSetIDX, 1:2]
      
      predictions <- predict(r, Round = FALSE, s = s, clamp = TRUE)
      
      er <- d@data$score[testSetIDX] - predictions
      
      
    }else{
      
      
      
      s <- sapply(testSetIDX, function(t) if(t%%nusers != 0) {c(t%%nusers, t%/%nusers + 1)}else{c(nusers, t%/%nusers)})
      s <- data.frame(user = s[1,], item = s[2,])
      
    
      #Last row mod nusers is 0, to avoid error i subtitute them with the right line.
      s$user[which(s$user == 0)] <- nusers
    
      predictions <- predict(r, Round = FALSE, s = s, clamp = TRUE)

      er <- d@data[testSetIDX] - predictions
      
    }
  
    for (n in unique(s$user)) {
      
      #error on rating
      u <- which(s$user == n)
      #browser()
      users_rmse <- users_rmse + sqrt(mean(er[u]^2))
      users_mae <- users_mae + mean(abs(er[u]))
      
    }
    
    # derivate an average MAE and RMSE for the users
    uRMSE <- c(uRMSE, users_rmse/nusers)
    uMAE <- c(uMAE, users_mae/nusers)
    
    # calculation on global MAE and RMSE
    
    mae_i <- mean(abs(er))
    
    rmse_i <- sqrt(mean(er^2))
    
    gMAE <- c(gMAE, mae_i)
    gRMSE <- c(gRMSE, rmse_i)
    
    ex.time <- c(ex.time, as.numeric(Sys.time() - ptm, units = "secs"))
    
    cat("\nFold:", i, "/", folds, " elapsed. Time:", as.numeric(Sys.time() - ptm, units = "secs"), "\n\n")
    
  }
  
  # average on folds
  uRMSE <- c(uRMSE, mean(uRMSE))
  uMAE <- c(uMAE, mean(uMAE))
  gRMSE <- c(gRMSE, mean(gRMSE))
  gMAE <- c(gMAE, mean(gMAE))
  ex.time <- c(ex.time, mean(ex.time))
  
  names(uRMSE) <- c(paste0(1:folds, rep("-fold", folds)), "Average")
  names(uMAE) <- c(paste0(1:folds, rep("-fold", folds)), "Average")
  names(gRMSE) <- c(paste0(1:folds, rep("-fold", folds)), "Average")
  names(gMAE) <- c(paste0(1:folds, rep("-fold", folds)), "Average")
  names(ex.time) <- c(paste0(1:folds, rep("-fold", folds)), "Average")
  
  show(r)
  
  cat("\n")
  
  as.data.frame(list(
    MAE = uMAE, 
    RMSE = uRMSE, 
    globalMAE = gMAE, 
    globalRMSE = gRMSE, 
    Time = ex.time ))
  
}) 











