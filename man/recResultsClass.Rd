\name{recResultsClass}
\docType{class}
\alias{recResultsClass}
\alias{recResultsClass-class}
\alias{show,recResultsClass-method}
\alias{[,recResultsClass,ANY,missing,missing-method}
\title{
Results of a recommendation.
}
\description{
Defines a structure for the results of a recommendation algorithm.
}
\section{Slots}{
  \describe{
    \item{\code{indices}:}{indices of the recommended items for each user, class \code{"list"}.}
    \item{\code{recommended}:}{names, if aviable, of the recommended items for each user, class \code{"list"}.}
   }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "recResultsClass")}
  \item{\code{[}}{signature(object = "dataSet", i = "ANY""): returns a subset of the recommended items.}
  }
}
\examples{
x <- matrix(sample(c(0:5), size = 100, replace = TRUE, 
  prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

colnames(x) <- paste0(rep("item-", 5), 1:5)
rownames(x) <- paste0(rep("user-", 20), 1:20)

x <- defineData(x)

model <- rrecsys(x, alg = "funk", k = 2, gamma = 0.1, lambda = 0.001)

rec <- recommend(model, topN = 1)

rec

rec[c(1,4,6)]

}
