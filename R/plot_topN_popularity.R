plot_topN_popularity <- function(topN, 
                              freq_count, 
                              x_label ="Position N", 
                              y_label = "Popularity"){
  

  df<-data.frame(occ=freq_count, idx=c(1:topN))
  
  disegno<-ggplot(df, aes(x=df$idx, y=df$occ)) + 
    geom_line() +
    geom_point() +
    theme_minimal()+ 
    theme(legend.position="none",text=element_text(family="mono"), panel.border = element_rect(fill = NA,colour = "grey50"))
  disegno<-disegno+
    labs(x=x_label, y=y_label )
  
  disegno
  
  
}


