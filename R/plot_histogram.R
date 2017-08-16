histogram<-function(data,title="", x="Rating values", y="# of ratings"){
  
  if(class(data) == "dataSet"){
    m <- data@data[!is.na(data@data)]
  }else if(class(data) == "sparseDataSet"){
    m <- data@data[,3]
  }else{
    stop()
  }
  
  
  
  qplot(m, geom="histogram", binwidth = 0.5)+
    theme_minimal()+
    theme(legend.position="none",text=element_text(family="mono"),panel.border = element_rect(fill = NA,colour = "grey50"))+
    labs(title=title, x=x, y=y)
}
