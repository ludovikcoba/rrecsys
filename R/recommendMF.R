recommendMF <- function(model, topN = 3, pt){
  
  UseMethod("recommendMF", model)
  
}

recommendMF.UBclass <- function(model, topN = 3, pt) {
  
  
  if(missing(pt)) stop("Missing pt (positive threshold).")
  
  if (topN >= ncol(model@data)) 
    stop("topN value is larger than the number of items that can be recommended.")
  if (topN < 1) 
    stop("Not valid value for topN.")
  
  num_user <- nrow(model@data)
  
  num_item <- ncol(model@data)
  
  rec_indices <- list()
  
  if(class(model@data) == "sparseDataSet"){
    
    x <- model@data
    
    for(i in 1:num_user){
  
      counter <-c(rep(0,num_item))
      
      rated_indices_on_user <- x@data$item[x@userPointers[[i]]]
      
      counter[rated_indices_on_user] <- NA
      
      for(j in 1:model@neigh){
        
        similar_user <- model@sim_index_kNN[i,j]
        
        user_pointer <- x@userPointers[[similar_user]]
        
        user_items <- x@data$item[user_pointer]
        
        user_score <- x@data$score[user_pointer]
        
        items <- which(user_score >= pt)
        
        items <- user_items[items]
        
        counter[items] <- counter[items] + 1
        
      }
      
      rec_indices <- c(rec_indices, list(order(counter, decreasing = TRUE, na.last = TRUE)[1:topN]))
      
    }
   }else{

     x <- model@data@data
     
     for(i in 1:num_user){
       counter <-c(rep(0,num_item))
       counter[which(!is.na(x[i,]))] <- NA
       
       for(j in 1:model@neigh){
         similar_user <- model@sim_index_kNN[i,j]
         
         items <- which(x[similar_user,]>= pt)
         
         counter[items] <- counter[items] + 1
         
       }
       
       
       rec_indices <- c(rec_indices, list(order(counter, decreasing = TRUE, na.last = TRUE)[1:topN]))
       
       
       
    }
  }
    
  rec_indices
  
} 

#itembased MF####
recommendMF.IBclass <- function(model, topN = 3, pt) {
  
  
  if(missing(pt)) stop("Missing pt (positive threshold).")
  
  if (topN >= ncol(model@data)) 
    stop("topN value is larger than the number of items that can be recommended.")
  if (topN < 1) 
    stop("Not valid value for topN.")
  
  num_user <- nrow(model@data)
  num_item <- ncol(model@data)
  
  rec_indices <- list()
  
  if(class(model@data) == "sparseDataSet"){
    
    x <- model@data
    
    for(i in 1:num_user){
      
      counter <-c(rep(0,num_item))
      
      user_pointer <- x@userPointers[[i]]
                  
      user_items <- x@data$item[user_pointer]
      
      counter[user_items] <- NA
      
      user_score <- x@data$score[user_pointer]
      
      positive_items <- which(user_score >= pt)
      
      positive_items <- user_items[positive_items]
      
      for(j in positive_items){
        
        similar_items <- model@sim_index_kNN[j,]
        
        counter[similar_items] <- counter[similar_items] + 1
        
      }
      

      browser()
  rec_indices <- c(rec_indices, list(order(counter, decreasing = TRUE, na.last = TRUE)[1:topN]))
  
  
    }
  }else{

    x <- model@data@data
    
    for(i in 1:num_user){
      
      counter <- itemMFCount( model@sim_index_kNN, x[i,], pt)
      
      counter[which(!is.na(x[i,]))] <- NA

      rec_indices <- c(rec_indices, list(order(counter, decreasing = TRUE, na.last = TRUE)[1:topN]))
    }
  }
  
  rec_indices
  
}
