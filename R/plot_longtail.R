plot_longtail <- function(data, x_label="Items", y_label="# of ratings" )
  {
  

  cl<- sort(colRatings(data), decreasing = T)
  
  #building line_chart
  df<- data.frame(x=1:length(cl),value=cl)
  
  quant_on_x<-quantile(df$x) #points that divide the data set into four equal groups, each group comprising a quarter of the data
  quant_on_value<-quantile(df$value)#points that divide the data set into four equal groups, each group comprising a quarter of the data
  
  disegno<-ggplot(df, aes(x=df$x, y=df$value,colour=(df$value>quant_on_value[4]))) +
    geom_line(size=1.2)+
    theme_minimal()+
    theme(legend.position="none",text=element_text(family="mono"),panel.border = element_rect(fill = NA,colour = "grey50"))
  disegno<-disegno+
    geom_hline(yintercept = quant_on_value[4],linetype="twodash", alpha(0.3))+
    geom_vline(xintercept = quant_on_x[2],linetype="twodash", alpha(0.3))
  disegno<-disegno+
    labs(x=x_label, y=y_label)
  disegno
  
}
