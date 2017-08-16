#rankscore on a given user.

rankScore <- function(recommendedIDX, testSetIDX, alpha){
  
  #extract index of the hits
  match_TS <- which(recommendedIDX %in% testSetIDX)
  
  if(length(match_TS) == 0 ) return(0)
  
  rankscoreMAX <- getrankscoreMAX(length(match_TS), alpha)
  
  rankscore_user <- (match_TS - 1)
  rankscore_user <- -rankscore_user/alpha
  rankscore_user <- 2^rankscore_user
  rankscore_user <- sum(rankscore_user)
  
  rankscore_user/rankscoreMAX
}

getrankscoreMAX<- function(n,alpha){
  
  rankscoreMAX <- 0
  
  rankscoreMAX <- 1/2^((c(1:n) - 1)/alpha)
  
  sum(rankscoreMAX)
}
