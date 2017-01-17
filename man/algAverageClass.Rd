\name{algAverageClass}
\docType{class}
\alias{algAverageClass}
\alias{algAverageClass-class}
\alias{show,algAverageClass-method}
\title{
Baseline algorithms exploiting global/item and user averages.
}
\description{
Container for the model learned using any average(global, user or item) based model.
}
\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{average}:}{average calculated either globally, on user or item, class \code{"matrix"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "algAverageClass")}
  }
}    
\seealso{
\code{\link{rrecsys}.}
}
