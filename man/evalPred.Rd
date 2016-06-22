\name{evalPred}
\alias{evalPred}
\alias{evalPred,evalModel-method}
\alias{evalPred,evalModel,list-method}
\title{
Evaluates the requested prediction algorithm.
}
\description{
Evaluates the prediction task of an algorithm with a given configuration and based on the given evaluation model. RMSE and MAE are both calculated individually for each user and then averaged over all users (in this case they will be referred as RMSE and MAE) as well as determined as the average error over all predictions (in this case they are named globalRMSE and globalMAE).
}
\usage{
evalPred(model, ...)
\S4method{evalPred}{evalModel}(model, alg, ... )
}

\arguments{
  \item{model}{
  Object of type \code{evalModel}. See \code{\link{evalModel-class}.}
}
  \item{alg}{
  The algorithm to be used in the evaluation. Of type \code{character}. 
}
  \item{\dots}{
  other attributes specific to the algorithm to be deployed. Refer to \code{\link{rrecsys}}.
}
}

\value{Returns a data frame with the \code{RMSE}, \code{MAE}, \code{globalRMSE} and \code{globalMAE} for each of the k-folds definded in the evaluation model and an average over all folds.}
\references{
F. Ricci, L. Rokach, B. Shapira, and P. B. Kantor, editors. \emph{Recommender Systems Handbook}. Springer, 2011. ISBN 978-0-387-85819-7. URL \url{http://www.springerlink.com/content/978-0-387-85819-7}.
}

\seealso{
  \code{\link{evalModel-class}}, \code{\link{rrecsys}}.
}
\examples{
  x <- matrix(sample(c(0,1), size = 200, replace = TRUE, 
        prob = c(.8,.2)), nrow = 20, byrow = TRUE)
  
  x <- defineData(x)
        
  e <- evalModel(x, 5)
  
  SVDEvaluation <- evalPred(e, "FunkSVD", k = 4) 
  
  SVDEvaluation         
  
  
  IBEvaluation <- evalPred(e, "IBKNN", neigh = 5)
  
  IBEvaluation
  
}
