setMethod("evalRec", 
          signature = c(model = "evalModel"), 
          function(model, 
                   alg = NULL, 
                   topN = 3,
                   topNGen = "hpr",
                   positiveThreshold = NULL, 
                   alpha = 10,
                   ...) {
            
            
            
            if (missing(alg)) {
              stop("Evaluation on recommendations can not proceed if argument alg and is not specified.")
            }
            
            if (model@data@binary) {
              if (!is.null(positiveThreshold)) {
                cat("NOTE: The \"positiveThreshold\" attribute is not needed for binary dataset.\n")
              }
              positiveThreshold <- 1
            }
            
            if (missing(positiveThreshold)) {
              stop("Evaluation on recommendations can not proceed if argument positiveThreshold and is not specified.")
            }
            
            if (!(topNGen %in% c("hpr", "mf"))) stop("Wrong topNGen value! Choose among: \"hpr\", \"mf\" (available only for IBKNN and UBKNN). ")
            
            cat("Evaluating top-", topN, " recommendation with ", rrecsysRegistry$get_entry(alg = alg)$alg, ".\n")
            
            nusers <- nrow(model@data)
            nitems <- ncol(model@data)
            
            # initialize empty containers
            nDCG <- rep(0, model@folds)
            rankscore <- rep(0, model@folds)
            
            TP_count <- rep(0, ncol(model@data))
            rec_counts <- rep(0, ncol(model@data))
            rec_popularity <- rep(0, topN)
            
            res <- NULL
            
            
            ex.time <- c()
            
            # iterations on folds
            for (i in 1:model@folds) {
              
              ptm <- Sys.time()
              # get a copy of the rating matrix
              d <- model@data
              # generate the training set
              testSetIDX <- model@fold_indices[[i]]
              #clear a dataset from the test set.
              x <- removeScores(d, model@fold_indices[[i]])

              popularity <- colRatings(x)
              
              r <- rrecsys(x, alg = alg, ...)

              #FIX ME: predict over single user and evaluate.####
              # get the recommended indices####
              if(topNGen == "hpr"){#if HPR
                #rec <- recommendHPR(x, alg, topN, ...)
               
                p <- predict(r, Round = FALSE, clamp = FALSE)
                rec <- lapply(1:nrow(d), 
                                      function(i) 
                                        order(p[i, ], na.last = NA, decreasing = T)[1:topN])
                
              }else if (topNGen == "mf"){
                rec <- recommendMF(r, topN, positiveThreshold)
              }
            
              # item and user coverage calculation
              tot_rec <- 0
              
          
              
              
              item_coverage <- rep(FALSE, ncol(d))
              
              res_on_fold <- list(TP = 0, 
                                  FP = 0, 
                                  TN = 0, 
                                  FN = 0, 
                                  precision = 0, 
                                  recall = 0, 
                                  F1 = 0)
              
              for (user in 1:nusers) {
                
                rec_counts[rec[[user]]] <-  rec_counts[rec[[user]]] + 1

                rec_popularity <- rec_popularity + popularity[rec[[user]]]
                
                #item coverage  
                for (n in rec[[user]]) {
                  item_coverage[n] <- TRUE
                  tot_rec <- tot_rec + 1
                }
                
                if(class(model@data) == "sparseDataSet"){
                  
                  user_vector <- c(rep(NA,nitems))
                  
                  user_items <- d@data$item[d@userPointers[[user]]]
                  
                  user_scores <- d@data$score[d@userPointers[[user]]]
                  
                  user_vector[user_items] <- user_scores
                  
                  indices_user <- d@data$item[model@fold_indices_x_user[[user]][[i]]]
                  
                } else{
                  
                  user_vector <- d@data[user, ]
                  
                  indices_user <- model@fold_indices_x_user[[user]][[i]]

                }

                #determine results on user. 
                res_user <- getPrecRecall(user_vector, 
                                          rec[[user]], 
                                          indices_user, 
                                          positiveThreshold,
                                          TP_count)
                
                TP_count <- res_user$TP_count
                
                for(j in 1:length(res_on_fold)) {
                  res_on_fold[[j]] <- res_on_fold[[j]] + res_user[[j]]
                }
                

                nDCG[i] <- nDCG[i] + 
                  eval_nDCG(rec[[user]], indices_user)
                
                rankscore[i] <- rankscore[i] + 
                  rankScore(rec[[user]], indices_user, alpha)
              }
              
              res_on_fold <- lapply(res_on_fold, function(temp) temp/nusers)
              
              # find average values for the confusion matrix.
              res <- rbind(res, as.data.frame(res_on_fold))
              #get averages
              nDCG[i] <- nDCG[i]/nusers
              rankscore[i] <- rankscore[i]/nusers
              
              ex.time <- c(ex.time, as.numeric(Sys.time() - ptm, units = "secs"))
              
              cat("\nFold:", i, "/", model@folds, " elapsed. Time:", as.numeric(Sys.time() - ptm, units = "secs"), "\n\n")
              
              
            }
            
            #output results####
            new('evalRecResults',
                  data = model@data,
                  alg = r@alg,
                  topN = topN,
                  topNGen = topNGen,
                  positiveThreshold = positiveThreshold, 
                  alpha = 10,
                  parameters = r@parameters,
                  TP = res$TP, 
                  FP = res$FP, 
                  TN = res$TN, 
                  FN = res$FN, 
                  precision = res$precision, 
                  recall = res$recall, 
                  F1 = res$F1,
                  nDCG = nDCG, 
                  rankscore = rankscore, 
                  item_coverage = 100 * sum(item_coverage)/ncol(model@data),
                  user_coverage = 100 * tot_rec/(topN * nrow(model@data)),
                  ex.time = ex.time,
                  TP_count = TP_count,
                rec_counts = rec_counts/model@folds,
                rec_popularity = rec_popularity/(nusers * model@folds)
            )
            
          })




setMethod("show", signature(object = "evalRecResults"), function(object) {

  res <- cbind(object@precision, object@recall, object@ex.time)
  
  res <- rbind(res, colMeans(res))
  
  cat("Algorithm: ", object@alg, "\n")
  cat("Configuration: \n")
  print(as.data.frame(object@parameters))
  
  rownames(res) <- c(paste0(1:length(object@precision), rep("-fold", length(object@precision))), "Average")
  colnames(res) <- c("precision", "recall", "ex.time")

  cat("\n")
  
  print(res)
})


setMethod("results", signature(object = "evalRecResults"), function(object, metrics = c(
  "TP", 
  "FP", 
  "TN", 
  "FN", 
  "precision", 
  "recall", 
  "F1",
  "nDCG", 
  "rankscore", 
  "ex.time"
)) {
  
  
  printable_results <- NULL
  
  for(metric in metrics){
    
    if(metric == "TP"){
      
      printable_results <- object@TP
      
    }
    
    if(metric == "FP"){
      
      printable_results <- cbind(printable_results, object@FP)
     
    }
    
    if(metric == "TN"){
      
        printable_results <- cbind(printable_results, object@TN)
        
    }
    
    if(metric == "FN"){
     
      printable_results <- cbind(printable_results, object@FN)
     
    }
    
    if(metric == "precision"){
     
      printable_results <- cbind(printable_results, object@precision)

    }
    
    if(metric == "recall"){
     
      printable_results <- cbind(printable_results, object@recall)
      
    }
    
    if(metric == "F1"){
     
      printable_results <- cbind(printable_results, object@F1)
      
    }
    
    if(metric == "nDCG"){
     
      printable_results <- cbind(printable_results, object@nDCG)
      
    }
    
    if(metric == "rankscore"){
    
      printable_results <- cbind(printable_results, object@rankscore)
      
    }
    
    if(metric == "ex.time"){
     
      printable_results <- cbind(printable_results, object@ex.time)
      
    }
  }
  
  cat("Algorithm: ", object@alg, "\n")
  cat("Configuration: \n")
  
  print(as.data.frame(object@parameters))
  
  printable_results <- rbind(printable_results, colMeans(printable_results))
  
  rownames(printable_results) <- c(paste0(1:length(object@precision), rep("-fold", length(object@precision))), "Average")
  colnames(printable_results) <- metrics
  
  cat("\n")
  
  print(printable_results)
  
})


