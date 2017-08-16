# ALS####
setMethod("predict", signature = c(model = "wALSclass"), function(model, Round = FALSE, s, clamp = FALSE) {
  

  
  if(missing(s)){
    item_not_rated <- which(is.na(model@data@data))
    
    # generate predictions
    p <- model@factors$U %*% t(model@factors$V)
    p <- p * model@weightScheme
    
    # replacing not rated items
    model@data@data[item_not_rated] <- p[item_not_rated]
    
    return(roundData(model@data, Round))
  }else{
    
    p <- NULL
    
    for(i in 1:nrow(s)){
      
      pred <- sum(model@factors$U[s[i,1], ] * model@factors$V[s[i,2], ])
      
      p <- c(p, pred)
      
    }
    
    return(p)
    
  }
  
  

  
})
# bpr####
setMethod("predict", signature = c(model = "BPRclass"), function(model, Round = FALSE, s,clamp = FALSE) {
  
  if(missing(s)){
    
    item_not_rated <- which(is.na(model@data@data))
    
    # generate predictions
    p <- model@factors$U %*% t(model@factors$V)
    
    model@data@data[item_not_rated] <- p[item_not_rated]
    
    return(roundData(model@data, Round))
    
  }else{
    
    p <- NULL
    
    for(i in 1:nrow(s)){
      
      pred <- sum(model@factors$U[s[i,1], ] * model@factors$V[s[i,2], ])
      
      p <- c(p, pred)
      
    }
    
    return(p)
  }
  
  

})

# average####

setMethod("predict", signature = c(model = "algAverageClass"), function(model, Round = FALSE, s,clamp = FALSE) {
  

  if(missing(s)){
    
    item_not_rated <- which(is.na(model@data@data))
    
    model@data@data[item_not_rated] <- model@average[item_not_rated]
  
    return(roundData(model@data, Round))

  }else{
    
    p <- NULL
    
    for(i in 1:nrow(s)){
      
      p <- c(p, model@average[s[i,1], s[i,2]])
      
    }
    
    return(p)
    
  }
  

}) 




#roundData############
roundData <- function(data, Round){
  if(Round){
    if (!data@binary & data@intScale)
    {
      data@data <- round(data@data * 2)/2
    }else{
      data@data <- round(data@data)
    }
    
  }
  data@data
}




