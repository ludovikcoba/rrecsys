#evalchart####
evalChart <- function(res, x = "items", y = "TP", x_label, y_label, y_lim){

  
  if (x == "items"){
    if (y == "TP"){

      if(missing(y_lim)){
        y_lim <- max(res@TP_count) + 0.1*max(res@TP_count)
      } 
      
      if(missing(y_label)){
        plot_PopDist_freq(res@data, res@TP_count, y_lim = y_lim)
      }else{
        plot_PopDist_freq(res@data, res@TP_count, y_label = y_label, y_lim = y_lim)
      }
      
    } else if(y == "num_of_recommendations"){
      
      if(missing(y_lim)){
        y_lim <- max(res@rec_counts) + 0.1*max(res@rec_counts)
      } 

      if(missing(y_label)){
        plot_PopDist_freq(res@data, res@rec_counts, y_lim = y_lim)
      }else{
        plot_PopDist_freq(res@topN, res@rec_counts, y_label = y_label, y_lim = y_lim)
      }
      
      
    } else stop("Wrong \"y\"!")
  }else if(x == "topN"){
    
    if(missing(y_lim)){
      y_lim <- max(res@rec_popularity) + 0.1*max(res@rec_popularity)
    } 
    
    
    if(y == "num_of_ratings"){
      
      if(missing(y_label)){
        plot_topN_popularity(res@topN, res@rec_popularity,  y_lim = y_lim)
      }else{
        plot_topN_popularity(res@topN, res@rec_popularity, y_label = y_label,  y_lim = y_lim)
      }
      
      
    } else stop("Wrong \"y\"!")
  }
  
}

#datachart####

dataChart <- function(data, x = "items", y = "num_of_ratings"){
  
  if(x == "items"){
    if(y == "num_of_ratings"){
      
      plot_longtail(data)
      
    }
  }
  
}