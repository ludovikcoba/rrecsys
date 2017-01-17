\name{getAUC}
\alias{getAUC}
\alias{getAUC,evalModel-method}
\alias{getAUC,evalModel}
\title{
Returns the Area under the ROC curve.
}
\description{
Computes the Area Under the ROC curve for a recommendation task of an algorithm with its given configuration and based on the given evaluation model.  
}
\usage{
getAUC(model, ...)
\S4method{getAUC}{evalModel}(model, alg, ... )
}

\arguments{
  \item{model}{
  Object of type \code{evalModel}. See \code{\link{evalModel-class}.}
}
  \item{alg}{
  The algorithm to be used in the evaluation. Of class \code{character}. 
}
  \item{\dots}{
  other attributes specific to the algorithm to be deployed. Refer to \code{\link{rrecsys}}.
}
}

\value{Returns a data frame with the \code{AUC} for each of the k-folds defined in the evaluation model and the overall average.}
\references{
T. Fawcett, \emph{“ROC Graphs: Notes and Practical Considerations for Data Mining Researchers ROC Graphs : Notes and Practical Considerations for Data Mining Researchers,”}, HP Inven., p. 27, 2003.
}

\seealso{
  \code{\link{evalModel-class}}, \code{\link{rrecsys}}.
}
\examples{
  x <- matrix(sample(c(NA, 1:5), size = 200, replace = TRUE, 
        prob = c(.6,.8,.8,.8,.8,.8)), nrow = 20, byrow = TRUE)
  
  x <- defineData(x)
        
  e <- evalModel(x, 5)
  
  auc <- getAUC(e, "FunkSVD", k = 4) 
  
  auc         
  
  
}
  