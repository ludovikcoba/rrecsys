defineData <- function(data, 
                       binary = FALSE, 
                       minimum = 1, 
                       maximum = 5, 
                       halfStar = FALSE, 
                       goodRating ) {
  
  data[is.nan(data)] <- NA
  data[is.null(data)] <- NA
  

  if (!binary) {
    
    v <- max(data, na.rm = TRUE)
    if(v > maximum) {
      warning("There are values larger that the defined threshold. If you want to keep using them please redefine the \"maximum\" argument.\n")
    }
    
    v <- min(data, na.rm = TRUE)
    if(v < minimum) {
      warning("There are values smaller that the defined threshold. If you want to keep using them please redefine the \"minimum\" argument.\n")
    }
    
    #ratings not in the range will be considered as missing rating.
    data[data < minimum] <- NA
    data[data > maximum] <- NA
    
  } else {
    
    data[data < goodRating] <- NA
    data[data >= goodRating] <- 1
    halfStar <- FALSE
    
  }
  
  new("dataSet", 
      data = data, 
      binary = binary, 
      minimum = minimum, 
      maximum = maximum, 
      halfStar = halfStar)
} 

#dataSet####
setMethod("show", signature(object = "dataSet"), function(object) {
  if (!object@binary) {
    cat("Dataset ")
  } else {
    cat("Binary dataset ")
  }
  cat("containing", nrow(object@data), "users and ", ncol(object@data), "items.")
})


#show dataSet####
setMethod("show", 
          signature(object = "dataSet"), 
          function(object) {
            
            if (!object@binary) {
              cat("Dataset ")
            } else {
              cat("Binary dataset ")
            }
            
            cat("containing", nrow(object@data), "users and ", ncol(object@data), "items.")
            
          })

# dataSet####
setAs("dataSet", "matrix", function(from) as(from@data, "matrix"))


#dim####
setMethod("dim", signature(x = "dataSet"), function(x) dim(x@data))


#ncol####
setMethod("ncol", signature(x = "dataSet"), function(x) ncol(x@data))


#nrow####
setMethod("nrow", signature(x = "dataSet"), function(x) nrow(x@data))


#colRatings####
setMethod("colRatings", signature(x = "dataSet"), function(x) {
  
  s <- apply(x@data, 2, function(j) sum(!is.na(j)))
  
  s
  
})


#colMeans####
setMethod("colMeans", signature(x = "dataSet"), function(x, damping) {
  
  if(missing(damping)){
    
    colSums(x@data, na.rm = TRUE)/colRatings(x)
    
  }else{
    
    colSums(x@data, na.rm = TRUE)/(colRatings(x) + damping)
    
  }
  
})


#rowRatings####
setMethod("rowRatings", signature(x = "dataSet"), function(x) {
  
  s <- apply(x@data, 1, function(i) sum(!is.na(i)))
  
  s
  
})

#rowMeans####
setMethod("rowMeans", signature(x = "dataSet"), function(x, damping) {
  
  if(missing(damping)){
    
    rowSums(x@data, na.rm = TRUE)/rowRatings(x)
    
  }else{
    
    rowSums(x@data, na.rm = TRUE)/(rowRatings(x) + damping)
    
  }
})

#numRatings####
setMethod("numRatings", signature(x = "dataSet"), function(x) {
  
  sum(!is.na(x@data))
  
})


#[####
setMethod("[", signature(x = "dataSet", i = "ANY", j = "ANY", drop = "missing"), function(x, i, j, ..., drop) {
  
  if (missing(i)) 
    i <- 1:nrow(x)
  if (missing(j)) 
    j <- 1:ncol(x)
  
  x@data <- x@data[i, j, ..., drop = FALSE]
  
  x
  
})


#spartsity####
setMethod("sparsity", signature(x = "dataSet"), function(x) {
  
  spars <- 1 - numRatings(x)/(nrow(x) * ncol(x))
  spars
  
})
