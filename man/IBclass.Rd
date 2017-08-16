\name{IBclass}
\docType{class}
\alias{IBclass}
\alias{IBclass-class}
\alias{show,IBclass-method}
\title{
Item based model.
}
\description{
Container for the model learned using any k-nearest neighbor item-based collaborative filtering algorithm.
}
\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{sim}:}{The item - item similarity matrix, class \code{"matrix"}.}
    \item{\code{sim_index_kNN}:}{The index of the k nearest neighbors for each item, class \code{"matrix"}.}
    \item{\code{parameters}:}{the parameters used in the model, class \code{"list"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "IBclass")}
  }
}    
\seealso{
\code{\link{rrecsys}.}
}
    
