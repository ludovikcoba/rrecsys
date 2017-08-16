defineData <- function(data, 
                       sparseMatrix = FALSE, 
                       binary = FALSE, 
                       minimum = 0.5, 
                       maximum = 5, 
                       intScale = TRUE, 
                       positiveThreshold = 3) {
    
  if(sparseMatrix){
    
    defineSparseMat(data,
                    binary, 
                    minimum, 
                    maximum, 
                    intScale, 
                    positiveThreshold)
                       
  }else{
    
    defineMat(data,
              binary, 
              minimum, 
              maximum, 
              intScale, 
              positiveThreshold)
                 
  }
    
} 
