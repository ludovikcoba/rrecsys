recommendHPR <- function(model, topN = 3) {
  
  if (topN >= ncol(model@data)) 
    stop("topN value is larger than the number of items that can be recommended.")
  if (topN < 1) 
    stop("Not valid value for topN.")


  if (model@alg == "Popular") {
    
    rec <- order(colRatings(model@data), decreasing = T)[1:topN]
    
    rec_indices <- lapply(1:nrow(model@data), function(m) rec)
    
    return(rec_indices)
    
  }
  
  
  p <- predict(model, Round = FALSE, clamp = FALSE)

  if(class(model@data) == "sparseDataSet"){
    rated_items <- model@data@data$user + nrow(model@data)*(model@data@data$item - 1)
    
  }else{
    
    rated_items <- which(!is.na(model@data@data))
    
  }
  
  
  

  p[rated_items] <- NA
  # here is where the ordering is done.
  rec_indices <- lapply(1:nrow(model@data), function(i) order(p[i, ], na.last = NA, decreasing = TRUE)[1:topN])
  
  rec_indices
}