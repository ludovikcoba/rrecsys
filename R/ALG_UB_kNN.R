setClass(
  
  "UBclass",
  
  representation(userAverage = "numeric"),
  
  contains = "SimilBasedClass"
)

UB_kNN <- function(data, simFunct = "cos", neigh = 10, coRatedThreshold = 20) {
  
  x <- data@data
  
  
 
  
  if (neigh > nrow(data) - 1) 
    stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")
  
  if (neigh < 1) 
    stop("Invalid value for neigh!!!")
  
  if(missing(simFunct)) simFunct <- "cos"
  
  ptm <- Sys.time()
  
  if(simFunct == "Pearson"){
    #remove the average ratings to compute the adjusted cosine.

    userPointers <- getPointers(1:nrow(data), data@data$user)
    userAv <- sapply(userPointers, function(i) sum(data@data$score[i]))/sapply(userPointers, length)
    userAv[which(is.na(userAv))] <- 0
    
    for(i in 1:nrow(data)) x$score[userPointers[[i]]] <- x$score[userPointers[[i]]] - userAv[i]
    
  }else if(simFunct != "cos"){
    stop("Wrong simFunct value. Choose among: \"cos\", \"adjCos\"")
  }
  
  # if none of the previous no modification won't be aplied on the data and we will compute the simple cosine similarity.

  
  sim <- UserSimilSparseMat(as.matrix(x), data@nrUsers, coRatedThreshold)
  
  #to avoid issues with ordering since simil method returns a similarity of 0 on the diagonal.
  diag(sim) <- -2
  
  colnames(sim) <- NULL 
  rownames(sim)<- NULL
  
  sim_index_kNN <- t(apply(sim, 1, function(q) 
    order(q, decreasing = TRUE, na.last = TRUE)))[, 1:neigh]
  
  # every item is similar 100% to itself. 
  diag(sim) <- NA
  
  cat("Neighborhood calculated in: ", as.numeric(Sys.time() - ptm, units = "secs"), "seconds.\n")
  
  if (neigh == 1) 
    sim_index_kNN <- as.matrix(sim_index_kNN)
  
  parameters <- list(
    simFunct = simFunct, 
    neigh = neigh
  )
  
  new("UBclass", 
      alg = "UBKNN", 
      sim = sim, 
      sim_index_kNN = sim_index_kNN, 
      neigh = neigh,
      userAverage = userAv, 
      parameters = parameters)
  

}

#Registry entry####
rrecsysRegistry$set_entry(alg = "UBKNN", 
                          fun = UB_kNN, 
                          description = "User based k-NN", 
                          reference = "B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.",
                          parameters = list(neigh = 2, simFunct = NA)) 

#Prediction method####

setMethod("predict", signature = c(model = "UBclass"), function(model, train_set, test_set, Round = FALSE, clamp = TRUE) {

  if(missing(test_set)){
    
    data <- matrix(NA, nrow = train_set@nrUsers, ncol = train_set@nrItems)
    
    idx <- nrow(train_set) * (train_set@data$item-1) + train_set@data$user
    
    data[idx] <- train_set@data$score
    
    # determining the set of items to be predicted
    item_not_rated <-which(is.na(data))

    s <- data.frame(user = item_not_rated%%train_set@nrUsers, item = item_not_rated%/%train_set@nrUsers + 1)
    #Last row mod nusers is 0, to avoid error i subtitute them with the right line.
    s$user[which(s$user == 0)] <- train_set@nrUsers

    p <- predictUB(
      data,
      as.matrix(s),
      model@sim_index_kNN,
      model@sim,
      model@neigh,
      train_set@maximum,
      train_set@minimum,
      model@userAverage,
      clamp
    )
    
    data[item_not_rated] <- p
    
    return(data)
    
  }else{
    
    test_set <- test_set %>% select(user,item)
    
    p <- predictUB(
      data@data,
      as.matrix(test_set),
      model@sim_index_kNN,
      model@sim,
      model@neigh,
      data@maximum,
      data@minimum,
      userMeans,
      clamp
    )
    
    return(p)
  }
  
  
  
})
