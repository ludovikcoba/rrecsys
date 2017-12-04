# Recall, precision, TP, FP, atc. on a given user.

getPrecRecall <- function(test_set, 
                          rec_items, 
                          goodRating,
                          TP_count,
                          catalog_size) {
  

  TP <- 0; FP <- 0; FN <- 0; TN <- 0; precision <- 0; recall <- 0; F1 <- 0; 

  
  if (length(rec_items) != 0) {
    # match test set items with recommended items
    
    TP_observations <-  test_set %>% 
      filter(score > goodRating, item %in% rec_items)
    
    TP <- nrow(TP_observations)
    
    TP_count[TP_observations$item] <- TP_count[TP_observations$item] + 1
    
    nr_positive_scores <- as.numeric(
      test_set %>% 
        filter(score >= goodRating) %>%
        summarise(n()))
    
    FN <- nr_positive_scores - TP
    
    FP <- length(rec_items) - TP
    
    TN <- catalog_size - TP - FN - FP
    

    precision <- TP/length(rec_items)
    
    if ((TP + FN) != 0){ 
      recall <- TP/(TP + FN)
    }else{
      recall <- 1
    }
    
    if ((precision + recall)!= 0 )
    {
      F1 = 2 * (precision * recall)/ (precision + recall)
    }
    
  }
  
  list(TP = TP, 
       FP = FP, 
       TN = TN, 
       FN = FN, 
       precision = precision, 
       recall = recall, 
       F1 = F1,
       TP_count = TP_count)
  
  
} 