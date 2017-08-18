# neigh: neighbourhood size.

# Reference: B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.
IB_kNN <- function(data, simFunct = "cos", neigh, coRatedThreshold = 20){
  
  UseMethod("IB_kNN", data)
}


IB_kNN.dataSet <- function(data, simFunct = "cos", neigh = 10, coRatedThreshold = 2) {

    x <- data@data
    
    ptm <- Sys.time()
    
    if (neigh >= ncol(x)) 
        stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")

    if (neigh < 1) 
        stop("Invalid value for neigh!!!")
    
    if(missing(simFunct)) simFunct <- "cos"
    
    #ADJUSTED COSINE SIMIL
    if(simFunct == "adjCos"){
      #remove the average ratings to compute the adjusted cosine.
      
      userAv <- rowAverages(data)
      
      for(i in 1:nrow(data)) x[i,] <- x[i, ] - userAv[i]
      
    }else if(simFunct != "cos"){
      stop("Wrong simFunct value. Choose among: \"cos\", \"adjCos\"")
    }

    sim <- ItemSimil(x, coRatedThreshold)

    #to avoid issues with ordering since simil method returns a similarity of 0 on the diagonal.
    diag(sim) <- NA
    
    colnames(sim) <- NULL 
    rownames(sim)<- NULL
    
    sim_index_kNN <- t(apply(sim, 1, function(q) 
      order(q, decreasing = TRUE, na.last = TRUE)))[,1:neigh]

    
    cat("Neighborhood calculated in: ", as.numeric(Sys.time() - ptm, units = "secs"), "seconds.\n")
    
    if (neigh == 1) 
        sim_index_kNN <- as.matrix(sim_index_kNN)
    
    parameters <- list(
      simFunct = simFunct, 
      neigh = neigh, 
      coRatedThreshold = coRatedThreshold
    )
    
    new("IBclass", 
        alg = "IBKNN", 
        data = data, 
        sim = sim, 
        sim_index_kNN = sim_index_kNN, 
        neigh = neigh,
        parameters = parameters)
    
}


#Sparse dataset #####

IB_kNN.sparseDataSet <- function(data, simFunct = "cos", neigh = 10, coRatedThreshold = 20) {

  x <- data@data
  
  if (neigh > nrow(data) - 1) 
    stop("Invalid value for neigh!!! Please change the neigh attribute.\nNeighborhood value is larger than the maximal value acceptable.")
  
  if (neigh < 1) 
    stop("Invalid value for neigh!!!")
  
  if(missing(simFunct)) simFunct <- "cos"
  
  ptm <- Sys.time()
  
  #ADJUSTED COSINE SIMIL
  if(simFunct == "adjCos"){
    #remove the average ratings to compute the adjusted cosine.
    userAv <- rowAverages(data)
    
    for(i in 1:nrow(data)) x$score[data@userPointers[[i]]] <- x$score[data@userPointers[[i]]] - userAv[i]
    
  }else if(simFunct != "cos"){
    stop("Wrong simFunct value. Choose among: \"cos\", \"adjCos\"")
  }

  x <- x[order(x$item,x$user),]

  sim <- ItemSimilSparseMat(as.matrix(x), ncol(data), coRatedThreshold)
  
  #to avoid issues with ordering since simil method returns a similarity of 0 on the diagonal.
  diag(sim) <- NA
  
  colnames(sim) <- NULL 
  rownames(sim)<- NULL
  
  sim_index_kNN <- t(apply(sim, 1, function(q) 
    order(q, decreasing = TRUE, na.last = TRUE)))[, 1:neigh]
  
  # every item is similar 100% to itself. 

  cat("Neighborhood calculated in: ", as.numeric(Sys.time() - ptm, units = "secs"), "seconds.\n")
  
  if (neigh == 1) 
    sim_index_kNN <- as.matrix(sim_index_kNN)
  
  parameters <- list(
    simFunct = simFunct, 
    neigh = neigh, 
    coRatedThreshold = coRatedThreshold
  )
  
  new("IBclass", 
      alg = "IBKNN", 
      data = data, 
      sim = sim, 
      sim_index_kNN = sim_index_kNN, 
      neigh = neigh,
      parameters = parameters)
}




#Prediction method####

setMethod("predict", signature = c(model = "IBclass"), function(model, Round = FALSE, s, clamp = TRUE) {
  
  if(class(model@data) == "sparseDataSet"){
    data <- convert2DataSet(model@data)
  }else{
    data <- model@data
  }
  

  nusers <- nrow(data)

  ItMeans <- colAverages(data)
  
  if(missing(s)){
    
    # determining the set of items to be predicted
    item_not_rated <-which(is.na(data@data))
    
    s <- data.frame(user = item_not_rated%%nusers, item = item_not_rated%/%nusers + 1)
    
    #Last row mod nusers is 0, to avoid error i subtitute them with the right line.
    s$user[which(s$user == 0)] <- nusers


    p <- predictIB(
      data@data,
      as.matrix(s),
      model@sim_index_kNN,
      model@sim,
      model@neigh,
      data@maximum,
      data@minimum,
      ItMeans,
      clamp
    )
    data@data[item_not_rated] <- p

    
  }else{

    p <- predictIB(
      data@data,
      as.matrix(s),
      model@sim_index_kNN,
      model@sim,
      model@neigh,
      data@maximum,
      data@minimum,
      ItMeans,
      clamp
    )

    return(p)
  }

  roundData(data, Round)
  
})


#Registry entry.####
rrecsysRegistry$set_entry(alg = "IBKNN", 
                          fun = IB_kNN, 
                          description = "Item based k-NN", 
                          reference = "B. Sarwar, G. Karypis, J. Konstan, and J. Riedl. Item-based collaborative filtering recommendation algorithms.",
                          parameters = list(simFunct = "cos", neigh = 10, coRatedThreshold = 2)) 



