setClass(

  "_ds",
  
  representation(
    
    binary = "logical",
    minimum = "numeric",
    maximum = "numeric",
    intScale = "logical"
    
    ),
  
  prototype(
    
    binary = FALSE,
    minimum = 1,
    maximum = 5,
    intScale = TRUE
    
  )
  
)


setGeneric(name = "sparsity", def = function(x) standardGeneric("sparsity"))

#spartsity####
setMethod("sparsity", signature(x = "_ds"), function(x) {
  
  spars <- 1 - numRatings(x)/(nrow(x) * ncol(x))
  spars
  
})

setMethod("summary", 
          signature(object = "_ds"), 
          function(object) {
            
            cat("# of items: ",ncol(object), "\n")
            cat("Minimum # of ratings per item: ", min(colRatings(object)), "\n")
            cat("Average # of ratings per item: ", mean(colRatings(object)), "\n")
            cat("Maximum # of ratings per item: ", max(colRatings(object)), "\n")

            cat("# of users: ",nrow(object), "\n")
            cat("Minimum # of ratings per user: ", min(rowRatings(object)), "\n")
            cat("Average # of ratings per user: ", mean(rowRatings(object)), "\n")
            cat("Maximum # of ratings per user: ", max(rowRatings(object)), "\n")

            cat("# of ratings: ",numRatings(object), "\n")
            
            cat("Average rating: ",averageRating(object), "\n")
            
            cat("Sparsity: ", sparsity(object), "\n")
            
          })




setMethod("show", 
          signature(object = "_ds"), 
          function(object) {
            
            if (!object@binary) {
              cat("Dataset ")
            } else {
              cat("Binary dataset ")
            }
            
            
            cat("containing", nrow(object), 
                "users and ", ncol(object), 
                "items and a total of ", numRatings(object), " scores.")
            
          })



#datset####
setClass(
  
  "dataSet",
  
  representation(
    
    data = "matrix"
    
    ),
  
  contains = "_ds"

)


#Sparse Mat####
setClass(
  
  "sparseDataSet", 
         
  representation(
    
    data = "data.frame", 
    userID = "numeric",
    itemID = "numeric",
    userPointers = "list",
    itemPointers = "list"
    
  ),
         
  contains = "_ds"

)

# SVDclass####
setClass(
  "SVDclass", 
         
  representation(
    
    alg = "character", 
    data = "_ds", 
    factors = "list", 
    parameters = "list", 
    baselines = "list"
    
  )
)

# Similarity based class####
setClass(
  
  "SimilBasedClass", 
  
         
  representation(
    
    alg = "character", 
    data = "_ds", 
    sim = "matrix", 
    sim_index_kNN = "matrix", 
    neigh = "numeric",
    parameters = "list"
    
  )
)

setClass(

  "IBclass",
  
  contains = "SimilBasedClass"
  
)

setClass(
  
  "UBclass",
  
  contains = "SimilBasedClass"
)



# wALSclass####
setClass(
  
  "wALSclass", 
  
  representation(
    
    alg = "character", 
    data = "dataSet", 
    factors = "list", 
    weightScheme = "matrix", 
    parameters = "list"

  )
)

# BPRclass####
setClass(
  "BPRclass",
  
  representation(
    
    alg = "character", 
    data = "dataSet", 
    factors = "list", 
    parameters = "list"
    
  )
)

# PPLclass####
setClass(
  
  "PPLclass", 
         
  representation(
    
    alg = "character", 
    data = "dataSet", 
    indices = "numeric",
    parameters = "list"
    
  )
)

# algAverageClass####
setClass(
  
  "algAverageClass", 
         
  representation(
    
    alg = "character", 
    data = "dataSet", 
    average = "matrix",
    parameters = "list"
    
  )
)

# evalModel####
setClass(
  "evalModel", 
         
  representation(
    
    data = "_ds", 
    folds = "numeric", 
    fold_indices = "list", 
    fold_indices_x_user = "list"
    
  )
)

#setClass(
  
#  "recResultsClass", 
         
#  representation(
    
#    indices = "list", 
#    recommended = "list"
    
#    )
#)


#evalResults#### 
setClass('evalRecResults', 
         representation(
           data = "_ds",
           alg = "character",
           topN = "numeric",
           topNGen = "character",
           positiveThreshold = "numeric", 
           alpha = "numeric",
           parameters = "list",
           TP = "numeric", 
           FP = "numeric", 
           TN = "numeric", 
           FN = "numeric", 
           precision = "numeric", 
           recall = "numeric", 
           F1 = "numeric",
           nDCG = "numeric", 
           rankscore = "numeric",
           item_coverage = "numeric",
           user_coverage = "numeric",
           ex.time = "numeric",
           TP_count = "numeric",
           rec_counts = "numeric",
           rec_popularity = "numeric"
         )) 
