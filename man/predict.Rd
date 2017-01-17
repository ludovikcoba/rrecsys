\name{predict}
\alias{predict}
\alias{predict,BPRclass-method}
\alias{predict,IBclass-method}
\alias{predict,algAverageClass-method}
\alias{predict,SVDclass-method}
\alias{predict,wALSclass-method}
\alias{predict, slopeOneclass}
\title{
Generate predictions.
}
\description{
Generate predictions on any of the previously trained models.
}
\arguments{
  \item{model}{
A previosly trained model, see \code{\link{rrecsys}}
}
  \item{Round}{
object of class \code{"logical"}, if \bold{TRUE} all the predictions are rounded to integer values, else values are returned as calculeted.}
}
\value{
All unrated items are predicted and the entire matrix is returned with the new ratings.
}

\seealso{
\code{\link{rrecsys}}, \code{\link{IBclass}}, \code{\link{SVDclass}}.
}
\examples{

data("mlLatest100k")

smallMl <- mlLatest100k[1:50, 1:100]

exExpl <- defineData(smallMl)

model1exp <- rrecsys(exExpl, alg = "funk", k = 10, gamma = 0.1, lambda = 0.001)

pre1 <- predict(model1exp, Round = TRUE) 
}