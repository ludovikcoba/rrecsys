plot_PopDist_freq <- function(data, 
                         freq_count, 
                         x_label ="Items by decreasing popularity", 
                         y_label = "Frequency",
                         y_lim ){
         
            idx<-order(colRatings(data), decreasing = T)
            occ<- freq_count[idx]
            ax_x<-c(1:ncol(data))
            
            df<-data.frame(occ=occ, idx=ax_x)
            
            disegno<-ggplot(df, aes(x=idx, y=occ)) + 
              ylim(0, as.integer(y_lim))+ 
              geom_point( colour="black",size = 1)+ 
              geom_point( colour="yellowgreen",size = .5)+
              theme_minimal()+ 
              theme(legend.position="none",text=element_text(family="mono"), panel.border = element_rect(fill = NA,colour = "grey50"))
            disegno<-disegno+
              labs(x=x_label, y=y_label)

            disegno
            
            
          }


