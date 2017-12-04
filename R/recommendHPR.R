recommendHPR <- function(model, data, topN = 3) {
  
  if (topN >= ncol(data)) 
    stop("topN value is larger than the number of items that can be recommended.")
  if (topN < 1) 
    stop("Not valid value for topN.")


  if (model@alg == "Popular") {
    
    rec <- order(colRatings(data), decreasing = T)[1:topN]
    
    rec_indices <- lapply(1:nrow(data), function(m) rec)
    
    return(rec_indices)
    
  }
  
  
  p <- predict(model, data, Round = FALSE, clamp = FALSE)

  if(class(data) == "sparseDataSet"){
    rated_items <- data@data$user + nrow(data)*(data@data$item - 1)
    
  }else{
    
    rated_items <- which(!is.na(data@data))
    
  }
  
  
  rec_indices <- lapply(1:data@nrUsers, function(x) NULL)
  
  p[rated_items] <- NA

  for(i in unique(data@data$user)){
    rec_indices[[i]] <- order(p[i, ], na.last = NA, decreasing = TRUE)[1:topN]
  }
  
  rec_indices
}