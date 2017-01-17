\name{setStoppingCriteria}
\alias{setStoppingCriteria}
\alias{showStoppingCriteria}
\alias{showDeltaError}

\title{
Set stopping criteria.
}
\description{
Define stopping criteria for functions that need a convergence check.
}
\usage{
setStoppingCriteria(autoConverge = FALSE, 
  deltaErrorThreshold = 1e-05, nrLoops = NULL, minNrLoops = 10)
showStoppingCriteria()
showDeltaError()

}

\arguments{
  \item{autoConverge}{
class \code{"logical"}, turns on the auto-convergence algorithm. 
}
  \item{deltaErrorThreshold}{
class \code{"numeric"}, is the threshold for the auto-convergence algorithm.
}
  \item{nrLoops}{
class \code{"numeric"}, number of loops that will be performed in case \code{autoConvergence} is \code{FALSE}
}

  \item{minNrLoops}{class \code{"numeric"}, the minimum number of loops to consider before verifying the deltaErrorThreshold.
}
}
\details{
If \code{autoConvergence = TRUE} tells the package to monitor the difference of global RMSE on two consecutive iterations, and to see if it drops below a threshold value. Whenever it drops under the specified value the iteration is considered converged. If \code{FALSE} the limit of iterations is delimited by \code{nrLoops}
}
\section{Methods}{
  \describe{
  \item{\code{showStoppingCriteria}}{Print on console the current configuration of the convergence algorithm.}
  \item{\code{showDeltaError}}{Report the delta error on each iteration of the algorithm that requires an auto-convergence algorithm.}
  }
}


\references{
M. D. Ekstrand, M. Ludwig, J. Kolb, and J. T. Riedl, \emph{“LensKit: a modular recommender framework,”}, Proc. fifth ACM Conf. Recomm. Syst. - RecSys ’11, p. 349, 2011.
}

\seealso{
See Also as \code{\link{rrecsys}}, \code{\link{SVDclass}}, \code{\link{wALSclass}}, \code{\link{BPRclass}}.
}
\examples{

setStoppingCriteria(autoConverge = TRUE)

setStoppingCriteria(nrLoops = 30)

}

