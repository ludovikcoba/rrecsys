calcMeans <- function(x) {
    
    totmean <- mean(x)
    devusermeans <- rowMeans(x) - totmean
    devitemmeans <- colMeans(x) - totmean
    biasmat <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    biasmat <- t(apply(biasmat, 1, function(temp) temp + devitemmeans))
    biasmat <- apply(biasmat, 2, function(temp) temp + devusermeans)
    biasmat <- biasmat + totmean
    
    list(devusermeans = devusermeans, devitemmeans = devitemmeans, totmean = totmean, biasmat = biasmat)
} 


baseline <- function(x){
  globalAv <- sum(x)/sum(x!=0)
  Va <- mean(apply(x,1,var))
  Vb <- var(x[x!=0])
  K <- Vb/Va
  # At some point of the diary Simon writes that he that he used a K = 25, even though he descibes the computation process of K as is computed above.
  # K <- 25 # 
  # On the diary this is called the better mean.
  baseline_items <- ((globalAv * K + apply(x, 2, sum))/(K + apply(x, 2, function(q) sum(q!=0))))
  #offset on user ratings
  offset <- apply(x, 1, function(q){
    i <- which(q!=0)
    sum(q[i] - baseline_items[i])
  })
  baseline_users <- (globalAv * K + offset)/(K + apply(x, 1, function(q) sum(q!=0)))
  #browser()
  list(globalAv = globalAv, baseline_users = baseline_users, baseline_items = baseline_items)
  
}