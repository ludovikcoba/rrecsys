# ref: Daniel Lemire, Anna MaclachlanSlope One Predictors for Online Rating-Based Collaborative Filtering.

slopeOne <- function(data) { 

  x <- data@data
  
  row_x <- nrow(data)
  col_x <- ncol(data)

  #compute the average deviation and cardinality for each pair of items.
  
  devcard <- weightedSlopeOneRM(x)
  
  new("slopeOneClass", alg = "slopeOne", data = data, devcard = devcard)
  
}

rrecsysRegistry$set_entry(alg = "slopeOne", # the algorithm bane for the dispacher
                          fun = slopeOne, # the algorithm function you crated above
                          description = "Weighted Slope One", # brief description, max one line
                          reference =  "Daniel Lemire, Anna MaclachlanSlope One Predictors for Online Rating-Based Collaborative Filtering.", # reference
                          parameters = NA) #argument with default values separated by comma.


setMethod("predict", signature = c(model = "slopeOneClass"), function(model, Round = FALSE) {
  
  data <- model@data
  x <- data@data
  minimum <- model@data@minimum
  maximum <- model@data@maximum
  
  Deviation <- model@devcard$Deviation
  Cardinality <- model@devcard$Cardinality
  
    p <- c()
    
    s <- s[order(s$user),]

    user <- -1
    
    for(i in 1:nrow(s)){
      if(s$user[i] != user){
        user <- s$user[i]
        ratedByU <- which(!is.na(x[user,]))
      }
      item <- s$item[i]
      
      denom <- sum(Cardinality[item, ratedByU])
      
      if(denom == 0){
        pr <- minimum
      }else{
        pr <- Deviation[item, ratedByU] + x[user, ratedByU]
        pr <- pr * Cardinality[item, ratedByU]
        pr <- sum(pr)/denom
      }
      
      if(pr < minimum) pr <- minimum
      if(pr > maximum) pr <- maximum
      
      p <- c(p, pr)
      
      }
    
    p

}) 
