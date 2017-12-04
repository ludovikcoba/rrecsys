setGeneric(name = "evalRec", def = function(rec, ...) standardGeneric("evalRec"))



setMethod("evalRec", 
          signature = c(rec = "list"), 
          function(rec, 
                   train, 
                   test,
                   topN = 3,
                   topNGen = "hpr",
                   positiveThreshold = NULL, 
                   alpha = 10,
                   ...) {
            
            
            
            if (train@binary) {
              if (!is.null(positiveThreshold)) {
                cat("NOTE: The \"positiveThreshold\" attribute is not needed for binary dataset.\n")
              }
              positiveThreshold <- 1
            }
            
            if (missing(positiveThreshold)) {
              stop("Evaluation on recommendations can not proceed if argument positiveThreshold and is not specified.")
            }
            
            if (!(topNGen %in% c("hpr", "mf"))) stop("Wrong topNGen value! Choose among: \"hpr\", \"mf\" (available only for IBKNN and UBKNN). ")
            
            nusers <- nrow(train)
            nitems <- ncol(train)
            
            # initialize empty containers
            nDCG <- 0
            rankscore <- 0
            
            TP_count <- rep(0, ncol(train))
            rec_counts <- rep(0, ncol(train))
            rec_popularity <- rep(0, topN)
            user_evaluated <- 0 #counts the number of users evaluated, that had a test set different from empty

            
            ex.time <- Sys.time()
            
            ############check carefully##############
            popularity <- train@data %>%
              arrange(item) %>%
              group_by(item) %>%
              summarise(Pop = length(user))
            
            popularity <- popularity$Pop
            #########################################
            item_coverage <- rep(FALSE, ncol(train))
            
            tot_res <- list(TP = 0, 
                                FP = 0, 
                                TN = 0, 
                                FN = 0, 
                                precision = 0, 
                                recall = 0, 
                                F1 = 0)
            

            users_unique_ids <- unique(test$user)
            userPointers <- getPointers(1:max(users_unique_ids), test$user)

            for (user_ID in users_unique_ids) {


              if(user_ID > train@nrUsers) next#new userID
              #if no recommendation was provided for the user it is skipped
              if(is.null(rec[[user_ID]])) next
              
              rec_counts[rec[[user_ID]]] <-  rec_counts[rec[[user_ID]]] + 1
              
              rec_popularity <- rec_popularity + popularity[rec[[user_ID]]]
              
              users_test_set <- test[userPointers[[user_ID]],]
              
              #skip if testset is empty or there is no positive rating.
              if(nrow(users_test_set) == 0) next
              if(nrow(users_test_set %>% filter(score >= positiveThreshold)) == 0) next
              
              user_evaluated <- user_evaluated + 1
              #item coverage  
              for (n in rec[[user_ID]]) {
                item_coverage[n] <- TRUE
              }

              #determine results on user. 
              res_user <- getPrecRecall(users_test_set, 
                                        rec[[user_ID]], 
                                        positiveThreshold,
                                        TP_count,
                                        nitems)
              
              TP_count <- res_user$TP_count
              
              for(j in 1:length(tot_res)) {
                tot_res[[j]] <- tot_res[[j]] + res_user[[j]]
              }
              
              
              nDCG <- nDCG + 
                eval_nDCG(rec[[user_ID]], users_test_set$item)
              
              rankscore <- rankscore + 
                rankScore(rec[[user_ID]], users_test_set$item, alpha)
            }

            #average over the number of users that were evaluated
            tot_res <- lapply(tot_res, function(x) x/user_evaluated)
            
            # find average values for the confusion matrix.
            #get averages
            nDCG <- nDCG/user_evaluated
            rankscore <- rankscore/user_evaluated
            
            ex.time <- as.numeric(Sys.time() - ex.time, units = "secs")
            
            cat("Evaluation completed. Time:", as.numeric(Sys.time() - ex.time, units = "secs"), "\n\n")
            
            
            
            
            #output results####
            new('evalRecResults',
                topN = topN,
                positiveThreshold = positiveThreshold, 
                alpha = 10,
                TP = tot_res$TP, 
                FP = tot_res$FP, 
                TN = tot_res$TN, 
                FN = tot_res$FN, 
                precision = tot_res$precision, 
                recall = tot_res$recall, 
                F1 = tot_res$F1,
                nDCG = nDCG, 
                rankscore = rankscore, 
                item_coverage = 100 * sum(item_coverage)/ncol(train),
                user_coverage = 100 * user_evaluated/ nrow(train),
                ex.time = ex.time,
                TP_count = TP_count,
                rec_counts = rec_counts,
                rec_popularity = rec_popularity/user_evaluated
            )
            
          })




setMethod("show", signature(object = "evalRecResults"), function(object) {

  res <- c(object@precision, object@recall, object@ex.time)
  
  names(res) <- c("precision", "recall", "ex.time")

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
  
  cat("Configuration: \n")
  
  print(as.data.frame(object@parameters))
  
  names(printable_results) <- metrics
  
  cat("\n")
  
  print(printable_results)
  
})


