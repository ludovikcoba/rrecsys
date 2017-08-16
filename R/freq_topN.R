freq_topN<-function(data, topNlist, title="Recommendation of items over topN", x="Items by descending popularity", y="Appearance frequencies"){
  
  #(counter for frequency) of raccomandation
  occ<-rep(0,ncol(data))
  
  #count occurencies
  for( i in 1:length(topNlist)){#this is a top1-list!
    it<- topNlist[[i]]
    occ[ it ]<-occ[ it ]+1
  }
  
  ind<-order(colRatings(data), decreasing = TRUE)
  occ<-occ[ind]
  
  ax_x<-c(1:ncol(data))
  df<-data.frame(occ=occ, ind=ax_x)

  disegno<-ggplot(df, aes(x=ind, y=occ)) + 
    geom_point( colour="black",size = 1) + 
    geom_point(colour="skyblue2", size = .5)+ 
    theme_minimal()+ 
    theme(legend.position="none",text=element_text(family="mono"),panel.border = element_rect(fill = NA,colour = "grey50"))
  disegno<-disegno+
    labs(title=title, x=x, y=y)
  disegno
  
}

