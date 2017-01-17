setMethod("evalModel", signature = c(data = "dataSet"), function(data, folds) {
  
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

removeTestSet <- function(x, tsI){
  
  nusers <- nrow(x)
  
  x@data[tsI] <- NA
  
  x@data <- matrix(x@data, nusers)
  
  x
}