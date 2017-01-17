\name{slopeOneClass}

\alias{slopeOneClass}
\alias{slopeOneClass-class}
\alias{show,slopeOneClass-method}
\alias{predict,slopeOneClass-method}

\title{
Slope One model.
}

\description{
Container for the model learned using Slope One algorithm.
}

\section{Slots}{
  \describe{
    \item{\code{alg}:}{The algorithm denominator, of class \code{"character"}.}
    \item{\code{data}:}{the dataset used for training the model, class \code{"matrix"}.}
    \item{\code{devcard}:}{Deviation and Cardinality between columns, class \code{"list"}.}
  }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "SVDclass")}
  }
}
\seealso{
\code{\link{rrecsys}.}
}

