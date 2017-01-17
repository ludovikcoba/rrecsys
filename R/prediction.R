






#roundData############
roundData <- function(data, Round){
  if(Round){
    if (!data@binary & data@halfStar)
    {
      data@data <- round(data@data * 2)/2
    }else{
      data@data <- round(data@data)
    }
    
    data@data[data@data < data@minimum] <- data@minimum
    data@data[data@data > data@maximum] <- data@maximum
  }
  data@data
}




