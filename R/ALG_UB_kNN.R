UB_kNN <- function(data, simFunct, neigh, coRatedThreshold = 20){
  
  UseMethod("UB_kNN", data)
}

#Registry entry####
rrecsysRegistry$set_entry(alg = "UBKNN", 
                          fun = UB_kNN, 
                          description = "User based k-NN", 
                          reference = "B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.",
                          parameters = list(neigh = 2, simFunct = NA)) 


UB_kNN.dataSet <- function(data, simFunct = "cos", neigh = 10, coRatedThreshold = 2) {
  
  x <- data@data
  
  if (neigh > nrow(data) - 1) 
    stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")
  
  if (neigh < 1) 
    stop("Invalid value for neigh!!!")
  
  if(missing(simFunct)) simFunct <- "cos"
  
  ptm <- Sys.time()
  
  #ADJUSTED COSINE SIMIL
  if(simFunct == "Pearson"){
    userAv <- rowAverages(data)
    
    for(i in 1:nrow(data)) x[i, which(!is.na(x[i,]))] <- x[i, which(!is.na(x[i,]))] - userAv[i]
    
  }else if(simFunct != "cos"){
    stop("Wrong simFunct value. Choose among: \"cos\", \"adjCos\"")
  }

  # if none of the previous no modification won't be aplied on the data and we will compute the simple cosine similarity.
  sim <- UserSimil(x, coRatedThreshold)
  
  #to avoid issues with ordering since simil method returns a similarity of 0 on the diagonal.
  diag(sim) <- NA
  
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
      data = data, 
      sim = sim, 
      sim_index_kNN = sim_index_kNN, 
      neigh = neigh,
      parameters = parameters)
  
}

#sparse dataset
UB_kNN.sparseDataSet <- function(data, simFunct = "cos", neigh = 10, coRatedThreshold = 20) {
  
  x <- data@data
  
  if (neigh > nrow(data) - 1) 
    stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")
  
  if (neigh < 1) 
    stop("Invalid value for neigh!!!")
  
  if(missing(simFunct)) simFunct <- "cos"
  
  ptm <- Sys.time()
  
  if(simFunct == "Pearson"){
    #remove the average ratings to compute the adjusted cosine.
    userAv <- rowAverages(data)
    
    for(i in 1:nrow(data)) x$score[data@userPointers[[i]]] <- x$score[data@userPointers[[i]]] - userAv[i]
    
  }else if(simFunct != "cos"){
    stop("Wrong simFunct value. Choose among: \"cos\", \"adjCos\"")
  }
  
  # if none of the previous no modification won't be aplied on the data and we will compute the simple cosine similarity.
  
  sim <- UserSimilSparseMat(as.matrix(x), nrow(data), coRatedThreshold)
  
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
      data = data, 
      sim = sim, 
      sim_index_kNN = sim_index_kNN, 
      neigh = neigh,
      parameters = parameters)
  

}


#Prediction method####

setMethod("predict", signature = c(model = "UBclass"), function(model, Round = FALSE, s, clamp = TRUE) {
  
  if(class(model@data) == "sparseDataSet"){
    data <- convert2DataSet(model@data)
  }else{
    data <- model@data
  }
  
  
  nusers <- nrow(data)
  
  userMeans <- rowAverages(data)
  
  if(missing(s)){
    
    # determining the set of items to be predicted
    item_not_rated <-which(is.na(data@data))
    
    s <- data.frame(user = item_not_rated%%nusers, item = item_not_rated%/%nusers + 1)
    
    #Last row mod nusers is 0, to avoid error i subtitute them with the right line.
    s$user[which(s$user == 0)] <- nusers
    
    
    
    p <- predictUB(
      data@data,
      as.matrix(s),
      model@sim_index_kNN,
      model@sim,
      model@neigh,
      data@maximum,
      data@minimum,
      userMeans,
      clamp
    )
    
    data@data[item_not_rated] <- p
    
  }else{
    
    p <- predictUB(
      data@data,
      as.matrix(s),
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
  
  roundData(data, Round)
  
  
})
