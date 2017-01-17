# neigh: neighbourhood size.

# Reference: B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.

IB_kNN <- function(data, simFunct, neigh = 2) {
  
  x <- data@data
  
  ptm <- Sys.time()
  
  if (neigh > ncol(x)) 
    stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")
  
  if (neigh < 1) 
    stop("Invalid value for neigh!!!")
  
  if(missing(simFunct)) simFunct <- readline(prompt = "Specify the similarity function:\n1: Cosine.\n2: Adjusted Cosine. \n3: Pearson.")
  
  while(missing(simFunct)) simFunct <- readline(prompt = "Please make sure you required the right similarity function:")
  
  while(!(simFunct %in% c(1:3))) simFunct <-readline(prompt = "Please make sure you required the right similarity function:")
  
  #ADJUSTED COSINE SIMIL
  if(simFunct == 2){
    #remove the average ratings to compute the adjusted cosine.
    
    userAv <- rowMeans(data)
    
    for(i in 1:nrow(data)) x[i, which(x[i,] != 0)] <- x[i, which(x[i,] != 0)] - userAv[i]
    
  }
  
  #PEARSON SIMIL
  if(simFunct == 3){
    
    #dumping  <- 1
    itemAv <- colMeans(data)
    
    for(j in 1:ncol(data)) x[which(x[,j] != 0),j] <- x[which(x[,j] != 0), j] - itemAv[j]
    
  }
  
  sim <- ItemSimil(x)
  
  #to avoid issues with ordering since simil method returns a similarity of 0 on the diagonal.
  diag(sim) <- -2
  
  colnames(sim) <- NULL 
  rownames(sim)<- NULL
  
  sim_index_kNN <- t(apply(sim, 1, function(q) 
    order(q, decreasing = TRUE, na.last = TRUE)))[, 1:neigh]
  
  # every item is similar 100% to itself. 
  diag(sim) <- 1
  
  cat("Neighborhood calculated in: ", as.numeric(Sys.time() - ptm, units = "secs"), "seconds.\n")
  
  if (neigh == 1) 
    sim_index_kNN <- as.matrix(sim_index_kNN)
  
  new("IBclass", 
      alg = "IBKNN", 
      data = data, 
      sim = sim, 
      sim_index_kNN = sim_index_kNN, 
      neigh = neigh)
  
}


#Registry entry.####
rrecsysRegistry$set_entry(alg = "IBKNN", 
                          fun = IB_kNN, 
                          description = "Item based k-NN", 
                          reference = "B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.",
                          parameters = list(neigh = 2)) 

#Prediction method####

setMethod("predict", signature = c(model = "IBclass"), function(model, Round = FALSE) {
  
  data <- model@data
  
  num_row <- nrow(data)
  
  rated_items <- which(!is.na(data@data))
  
  item_not_rated <- vector("list", num_row)
  
  # determining the set of items to be predicted
  item_not_rated <- lapply(1:num_row, function(i) which(is.na(data@data[i, ])))
  
  # generating predictions for the requested items. Weighted sum computation.
  globaMean <- sum(data@data, na.rm = TRUE)/numRatings(data) 
  
  for (i in 1:num_row) {
    
    for (item in item_not_rated[[i]]) {
      
      #Scores on items in neighborhood of "item".
      sim_item_ratings <- data@data[i, model@sim_index_kNN[item, ]]
      
      # extract similarity values in the neighorhood of "item".
      item_sim <- model@sim[item, model@sim_index_kNN[item, ]] 
      
      denom_w_sum <- sum(abs(item_sim) * (!is.na(sim_item_ratings)))
      
      if (denom_w_sum == 0) {
        data@data[i, item] <- globaMean
      } else {
        data@data[i, item] <- sum(sim_item_ratings * item_sim , na.rm = TRUE)/denom_w_sum
      }
    }
  }
  
  roundData(data, Round)
  
})



