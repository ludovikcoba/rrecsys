
#evalModel on dataSet####
setMethod("evalModel", signature(data = "dataSet"), function(data, folds = 5){

    if (missing(folds)) 
        folds <- 5
    
    if (folds < 2) 
        stop("k-fold cross validation requires at least two folds to continue!")
    
    x <- data@data
    
    nr <- nrow(x)
    
    fold_indices <- vector("list", folds)
    
    fold_indices_x_user <- vector("list", nr)
    
    rated_index_by_row <- lapply(1:nr, function(temp) which(!is.na(x[temp, ])))
    
    for (i in 1:nr) {
      
        where <- sample(1:folds)
        
        userRatings <- sample(rated_index_by_row[[i]])
        
        splitList <- suppressWarnings(split(userRatings, where))
        
        fold_indices_x_user[[i]] <- splitList
        
        splitList <- lapply(splitList, function(v) (v-1)*nr + i)
        
        fold_indices <- lapply(1:folds, function(t) c(fold_indices[[t]], splitList[[t]]))
        
    }

    new("evalModel", data = data, folds = folds, fold_indices = fold_indices, fold_indices_x_user = fold_indices_x_user)
})

#evalModel sparseDataset####
setMethod("evalModel", signature(data = "sparseDataSet"), function(data, folds = 5){
            
            if (missing(folds)) 
              folds <- 5
    
            if (folds < 2) 
              stop("k-fold cross validation requires at least two folds to continue!")
            
            fold_indices <- vector("list", folds)
            
            fold_indices_x_user <- vector("list", nrow(data))
            
            for(i in 1:nrow(data)){
              
              userRatings <- data@userPointers[[i]]
              
              #random sorting
              userRatings <- sample(userRatings)
              
              where <- sample(1:folds)
              
              splitList <- suppressWarnings(split(userRatings, where))
              
              fold_indices <- lapply(1:folds, function(t) c(fold_indices[[t]], splitList[[t]]))
              
              fold_indices_x_user[[i]] <- splitList

            }
            
            new("evalModel", 
                data = data, 
                folds = folds, 
                fold_indices = fold_indices, 
                fold_indices_x_user = fold_indices_x_user)
          })

#remove testset ####
removeScores <- function(x, tsI){
  
  UseMethod("removeScores", x)
  stop("Wrong input method!")
  
  
}

removeScores.dataSet <- function(x, tsI){
  
  nusers <- nrow(x)
  
  x@data[tsI] <- NA
  
  x@data <- matrix(x@data, nusers)
  
  x
}

removeScores.sparseDataSet <- function(x, tsI){

  x@data <- x@data[-tsI, ]
  x@userPointers <- getPointers(1:nrow(x), x@data$user)
  x@itemPointers <- getPointers(1:ncol(x), x@data$item)

  x
  
}

