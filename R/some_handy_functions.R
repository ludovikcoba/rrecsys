setMethod("show", signature(object = "slopeOneClass"), function(object) {
  cat("The model was trained on the dataset using ", object@alg, "algorithm.")
})


setMethod("show", signature(object = "evalModel"), function(object) {
    if (object@data@binary) {
        cat(object@folds, "- fold cross validation model on the binary dataset with", nrow(object@data), "users and", ncol(object@data), "items.")
    } else {
        cat(object@folds, "- fold cross validation model on the dataset with", nrow(object@data), "users and", ncol(object@data), "items.")
        
    }
})


# recResultsClass####
setMethod("show", signature(object = "recResultsClass"), function(object) {
    print(object@recommended)
})

setMethod("[", signature(x = "recResultsClass", i = "ANY", j = "missing", drop = "missing"), function(x, i, j, ..., drop) {
    
    if (missing(i)) 
        i <- 1:nrow(x)
    
    
    x@recommended[i]
})




# show methods for the trained model####
setMethod("show", signature(object = "SVDclass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm.\nThe algorithm was configured with the following parameters:\n")
    print(as.data.frame(object@parameters))
})

setMethod("show", signature(object = "IBclass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm. \nThe algorithm was configured with the following neighborhood width:", object@neigh)
})

setMethod("show", signature(object = "wALSclass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm. \nThe algorithm was configured with the following parameters:\n")
    print(as.data.frame(object@parameters))
})

setMethod("show", signature(object = "BPRclass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm. \nThe algorithm was configured with the following parameters:\n")
    print(as.data.frame(object@parameters))
})

setMethod("show", signature(object = "PPLclass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm.")
})


setMethod("show", signature(object = "algAverageClass"), function(object) {
    cat("The model was trained on the dataset using ", object@alg, "algorithm.")
}) 
