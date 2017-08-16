defineMat <- function(data, 
                         binary, 
                         minimum, 
                         maximum, 
                      intScale, 
                          positiveThreshold)
{
  # Missing ratings will have NA values.
  data[is.nan(data)] <- NA
  data[is.null(data)] <- NA
  
  
  if (!binary) {
    
    v <- max(data, na.rm = TRUE)
    if(v > maximum) {
      cat("There are values larger that the defined threshold. If you want to keep using them please redefine the \"maximum\" argument.\n")
    }
    
    v <- min(data, na.rm = TRUE)
    if(v < minimum) {
      cat("There are values smaller that the defined threshold. If you want to keep using them please redefine the \"minimum\" argument.\n")
    }
    
    #ratings not in the range will be considered as missing rating.
    data[data < minimum] <- NA
    data[data > maximum] <- NA
    
  } else {
    
    data[data < positiveThreshold] <- NA
    data[data >= positiveThreshold] <- 1
    intScale <- FALSE
    
  }
  
  new("dataSet", 
      data = data, 
      binary = binary, 
      minimum = minimum, 
      maximum = maximum, 
      intScale = intScale)
  
}

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
setMethod("colAverages", signature(x = "dataSet"), function(x, damping = 0) {
  mean_score <- mean(x@data, na.rm = TRUE)
  
  m <- colSums(x@data - mean_score, na.rm = TRUE)/(colRatings(x) + damping)
  m[which(is.na(m))] <- 0
  m + mean_score
  
})


#rowRatings####
setMethod("rowRatings", signature(x = "dataSet"), function(x) {
  
  s <- apply(x@data, 1, function(i) sum(!is.na(i)))

  s
  
})

#rowMeans####
setMethod("rowAverages", signature(x = "dataSet"), function(x, damping = 0) {
  
  mean_score <- mean(x@data, na.rm = TRUE)
  m <- rowSums(x@data - mean_score, na.rm = TRUE)/(rowRatings(x) + damping)
  m[which(is.na(m))] <- 0
  m + mean_score
  
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

setMethod("averageRating", signature(x = "dataSet"), function(x) {
  
  sum(x@data, na.rm = TRUE)/numRatings(x)
  
})
