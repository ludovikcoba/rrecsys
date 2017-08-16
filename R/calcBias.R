calcMeans <- function(x) {
    
    totmean <- mean(x)
    devusermeans <- rowMeans(x) - totmean
    devitemmeans <- colMeans(x) - totmean
    biasmat <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    biasmat <- t(apply(biasmat, 1, function(temp) temp + devitemmeans))
    biasmat <- apply(biasmat, 2, function(temp) temp + devusermeans)
    biasmat <- biasmat + totmean
    
    list(devusermeans = devusermeans, devitemmeans = devitemmeans, totmean = totmean, biasmat = biasmat)
} 


baseline <- function(x){
  
  UseMethod("baseline", x)
  stop("Wrong inout on baseline method!")
  
}
  #####################
  #Needs FIX: make the dumping variable available to the user. Remember: zero values must not be permitted since it might gnerate a NaN.
  #Dumping
# this is Korens definition for the mean in MATRIX FACTORIZATION TECHNIQUES FOR RECOMMENDER SYSTEMS
  #minNumRating <- 1
  #colCounts[colCounts < minNumRating] <- colCounts[colCounts < minNumRating] + dumping
  #rowCounts[rowCounts < minNumRating] <- rowCounts[rowCounts < minNumRating] + dumping 
  #colCounts <- colCounts + dumping
  #rowCounts <- rowCounts + dumping
  #####################  

#baseline for matrix object####
baseline.dataSet <- function(x){
  
  dumping  <- 1
  M <- x@data
  
  globalAv <- sum(M, na.rm = TRUE)/numRatings(x)
  colCounts <- colRatings(x)
  rowCounts <- rowRatings(x)
  
  baseline_items <- (colSums(M, na.rm = TRUE) - globalAv * colCounts)/(colCounts + dumping)
  
  for(i in 1:NROW(x)) M[i, ] <- M[i, ] - baseline_items
  
  baseline_users <- (rowSums(M, na.rm = TRUE) - globalAv * rowCounts)/(rowCounts + dumping)
  
  # in case there is a user or item with 0 ratings the baseline is 0.
  
  baseline_users[is.nan(baseline_users)] <- 0
  
  baseline_items[is.nan(baseline_items)] <- 0
  
  list(globalAv = globalAv, baseline_users = baseline_users, baseline_items = baseline_items)
  
}

#baseline for sparse matrix####  
baseline.sparseDataSet <- function(x){
  
  dumping  <- 3
  
  globalAv <- sum(x@data$score)/numRatings(x)
  
  baseline_items <- sapply(x@itemPointers, function(i) sum(x@data$score[i] - globalAv))
  
  baseline_items <- baseline_items/(colRatings(x) + dumping)
  
  baseline_users <- sapply(x@userPointers, function(i) {
    sum(x@data$score[i] - baseline_items[x@data$item[i]] - globalAv)})
  
  baseline_users <- baseline_users/(rowRatings(x) + dumping)
  
  list(globalAv = globalAv, baseline_users = baseline_users, baseline_items = baseline_items)
  
}
