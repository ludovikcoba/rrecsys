\name{recommend}
\alias{recommend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate recommendation.
}
\description{
This method generates top-n recommendations based on a model that has been trained before. 
}
\usage{
recommend(model, topN = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
the trained model of any algorithm.
}
  \item{topN}{
number of items to be recommended per user, class \code{numeric}.
}
}


\value{
Returns a list with suggested items for each user.
}

\seealso{
\code{\link{rrecsys}}.
}
\examples{

myratings <- matrix(sample(c(0:5), size = 200, replace = TRUE, 
        prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

myratings <- defineData(myratings)

r <- rrecsys(myratings, alg = "FunkSVD", k = 2)

rec <- recommend(r)

}
