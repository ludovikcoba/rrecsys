\name{wALSclass}
\docType{class}
\alias{wALSclass}
\alias{wALSclass-class}
\alias{show,wALSclass-method}
\title{
Weighted Alternating Least Squares based model.
}
\description{
Container for the model learned using any weighted Alternating Least Squares based algorithm.
}
\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{factors}:}{user(U) and items(V) factors, class \code{"list"}.}
    \item{\code{weightScheme}:}{The weighting scheme used in updating the factors, class \code{"matrix"}.}
    \item{\code{parameters}:}{the parameters(such as number of factors \code{k}, learning rate \code{lambda}, number of iterations until \code{convergence} and the weighting scheme) used in the model, class \code{"list"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "wALSclass")}
  }
}    
\seealso{
\code{\link{rrecsys}.}
}
    
