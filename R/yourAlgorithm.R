# Step 3. Output Class Definition####
# You must create a specific class for your output. 
# You set a specific name for your class(it must be a string).
# In the representation you specify names of the slots and their class. 
# To get the class name on an object use the method class() on that item.
# Add here whatever slots you will need to compute predictions.
# You can create as many slots as you may need, just separate them by comma in the representation field.
setClass('outputClassName', representation( alg = "character", 
                                            data = "dataSet", 
                                            parameters = "list" )) 
setMethod("show", signature(object = "outputClassName"), function(object) {
  cat("The model was trained on the dataset using ", object@alg, "algorithm.\nThe algorithm was configured with the following parameters:\n")
  print(as.data.frame(object@parameters))
})


# Step 1. Defining the Algorithm. ####
# The data argument must not be edited as it is received from the dispacher.
# You may need to add the arguments that are required by the user.
yourAlgorithm <- function(data, firstP = 1, secondParam = "if string") { 
  #extract the rating matrix
  x <- data@data
  #remove possible names
  colnames(x) <- NULL
  rownames(x) <- NULL
  #you can get the number of rows and columns
  row_x <- nrow(x)
  col_x <- ncol(x)
  
  # use browser() function to single step
  browser()
  
  
  # Step 2. Coding the Algorithm ####
  ###################################################
  ###################################################
  #          The Algorithm Code Here                #
  ###################################################
  ###################################################
  
  
  
  
  # Step 3(again). Generating the output.####
  # We create a list with the values of the parameters specified by the user and feed it in the output class.
  # Complete this list with your arguments.
  param_list <- list(firstP = firstP, secondParam = secondParam)
  # Create the output object that will be feed to the prediction method.
  # Add here whatever slots you will need to compute predictions.
  # The slots must match.
  new("outputClassName", alg = "yourAlgorithm", data = data, parameters = param_list)
  
}

# Step 4. Add the new algorithm to the registry####
rrecsysRegistry$set_entry(alg = "yourAlgorithm", # the algorithm bane for the dispacher
                          fun = yourAlgorithm, # the algorithm function you crated above
                          description = "brief description", # brief description, max one line
                          reference =  "Reference if there is some", # reference
                          parameters = list(firstP = 1, secondParam = "if string")) #argument with default values separated by comma.



# Step 5. Prediction method####
setMethod("predict", signature = c(model = "outputClassName"), function(model, Round = FALSE) {
  
  # generate prediction on model@data
  ###################################################
  ###################################################
  #          Predict Algorithm Here                 #
  ###################################################
  ###################################################
  
  
  roundData(model@data, Round)
}) 



