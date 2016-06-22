\name{PPLclass}
\docType{class}
\alias{PPLclass}
\alias{PPLclass-class}
\alias{show,PPLclass-method}
\title{
Popularity based model.
}
\description{
Container for the model learned by an unpersonalized popularity-based algorithm.
}
\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{indices}:}{the indices of items ordered by popularity, class \code{"integer"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "PPLclass")}
  }
}    
\seealso{
\code{\link{rrecsys}.}
}