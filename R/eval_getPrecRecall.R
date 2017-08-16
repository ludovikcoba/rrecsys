# Recall, precision, TP, FP, atc. on a given user.

getPrecRecall <- function(data, 
                          rec_items, 
                          fold_items_x_user, 
                          goodRating,
                          TP_count) {
  
  res <- list(TP = 0, 
              FP = 0, 
              TN = 0, 
              FN = 0, 
              precision = 0, 
              recall = 0, 
              F1 = 0,
              TP_count = TP_count)
  
  if (length(rec_items) != 0) {
    # match test set items with recommended items

    match_TS <- which(rec_items %in% fold_items_x_user)
    
    for(i in rec_items[match_TS]){
      if (data[i] >= goodRating){
        res$TP_count[i] <- res$TP_count[i] + 1
        res$TP <- res$TP + 1
      }
    }
    
    #res$TP <- sum(data[rec_items[match_TS]] >= goodRating)

    res$FN <- sum(data[fold_items_x_user] >= goodRating) - res$TP
    
    res$FP <- length(rec_items) - res$TP
    
    res$TN <- length(data) - res$TP - res$FN - res$FP
    
    if (sum(data[fold_items_x_user] >= goodRating, na.rm =T) == 0) {#all the items in the fold disliked by the user
      res$precision <- 1
    }
    
    if (length(rec_items) != 0) {
      res$precision <- res$TP/length(rec_items)
    }else{
      res$precision <- 1
    }
    
    if ((res$TP + res$FN) != 0){ 
      res$recall <- res$TP/(res$TP + res$FN)
    }else{
      res$recall <- 1
    }
    
    if ((res$precision + res$recall)!= 0 )
    {
      res$F1 = 2 * (res$precision * res$recall)/ (res$precision + res$recall)
    }
    
  }
  
  res
  
} 