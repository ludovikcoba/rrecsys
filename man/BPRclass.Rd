\name{BPRclass}
\docType{class}
\alias{BPRclass}
\alias{BPRclass-class}
\alias{show,BPRclass-method}
\title{
Bayesian Personalized Ranking based model.
}
\description{
Container for the model learned using any Bayesian Personalized Ranking based model.
}
\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{factors}:}{user(U) and items(V) factors, class \code{"list"}.}
    \item{\code{parameters}:}{the parameters(such as number of factors \code{k}, learning rate \code{lambda}, user regularization term \code{regU}, positive rated item regularization term \code{regI}, negative rated item regularization term \code{regJ} and the Boolean \code{updateJ} to decide whatever negative updates are required) used in the model, class \code{"list"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "BPRclass")}
  }
}    
\seealso{
\code{\link{rrecsys}.}
}
    
