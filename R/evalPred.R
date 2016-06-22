setMethod("evalPred", signature = c(model = "evalModel"), function(model, alg = NULL, ...) {
    
    if (is.null(alg)) {
        stop("Evaluation on recommendations cannot proceed if argument alg and is not specified.")
    }
    
    nusers <- nrow(model@data)
    
    #user based RMSE & MAE
    uRMSE <- c()
    uMAE <- c()
    #global RMSE & MAE
    gRMSE <- c()
    gMAE <- c()
    
    for (i in 1:model@folds) {
        
        ptm <- Sys.time()
        
        copy_data <- model@data
        copy_data@data[model@fold_indices[[i]]] <- 0
        
        copy_data@data <- matrix(copy_data@data, nusers)
        
        r <- rrecsys(copy_data, alg = alg, ...)
        
        # mae & rmse on user
        predictions <- predict(r, Round = FALSE)
        
        # calculation on MAE and RMSE on each user
        users_rmse <- 0
        users_mae <- 0

        for (n in 1:nrow(model@data)) {
          #error on rating
          e <- model@data@data[n, model@fold_indices_x_user[[i]][[n]]] -
               predictions[n, model@fold_indices_x_user[[i]][[n]]]

          if(length(e) == 0) next
          
          users_rmse <- users_rmse + sqrt(mean(e^2))
          users_mae <- users_mae + mean(abs(e))
        }
        # derivate an average MAE and RMSE for the users
        uRMSE <- c(uRMSE, users_rmse/nusers)
        uMAE <- c(uMAE, users_mae/nusers)
        
        # calculation on global MAE and RMSE
        e <- model@data@data[model@fold_indices[[i]]] -
             predictions[model@fold_indices[[i]]]

        mae_i <- mean(abs(e))
        
        rmse_i <- sqrt(mean(e^2))
        
        gMAE <- c(gMAE, mae_i)
        gRMSE <- c(gRMSE, rmse_i)
        
  
        cat("\nFold:", i, "/", model@folds, " elapsed. Time:", as.numeric(Sys.time() - ptm, units = "secs"), "\n\n")

    }
    
    # average on folds
    uRMSE <- c(uRMSE, mean(uRMSE))
    uMAE <- c(uMAE, mean(uMAE))
    gRMSE <- c(gRMSE, mean(gRMSE))
    gMAE <- c(gMAE, mean(gMAE))
    

    names(uRMSE) <- c(paste0(1:model@folds, rep("-fold", model@folds)), "Average")
    names(uMAE) <- c(paste0(1:model@folds, rep("-fold", model@folds)), "Average")
    names(gRMSE) <- c(paste0(1:model@folds, rep("-fold", model@folds)), "Average")
    names(gMAE) <- c(paste0(1:model@folds, rep("-fold", model@folds)), "Average")
    
    show(r)
    cat("\n")
    as.data.frame(list(MAE = uMAE, RMSE = uRMSE, globalMAE = gMAE, globalRMSE = gRMSE))
    
}) 
