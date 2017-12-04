setGeneric(name = "evalSplit", def = function(data, test_ratio) standardGeneric("evalSplit"))


setMethod("evalSplit", signature(data = "sparseDataSet"), function(data, test_ratio = 0.25){

  test_id <- c()
  
  for(i in 1:nrow(data)){

    userRatings <- which(data@data$user == i)
    
    test_id <- c(test_id, 
                 sample(userRatings, 
                        size = test_ratio*length(userRatings))
                 )
    
  }
  
  train_set   <-  data@data[-test_id, ]
  test_set    <-  data@data[test_id, ]
  
  train_set <- new("sparseDataSet", 
               data = train_set,
               binary = data@binary,
               minimum = data@minimum,
               maximum = data@maximum,
               intScale = data@intScale,
               nrUsers = data@nrUsers,
               nrItems = data@nrItems)
  
  
  list(train = train_set, test = test_set)
  
})