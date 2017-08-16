\name{recommend}
\alias{recommendHPR}
\alias{recommendMF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate recommendation.
}
\description{
This method generates top-n recommendations based on a model that has been trained before. Two main methods: recommendHPR, recommendMF. The first method recommends the highest predicted ratings on a user. Instead recommendMF (currently available only for IBKNN and UBKNN), recommends the most frequent item in the user's neighborhood. 
}
\usage{
recommendHPR(model, topN = 3)
recommendMF(model, topN = 3, pt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
the trained model of any algorithm.
}
  \item{topN}{
number of items to be recommended per user, class \code{numeric}.
}

  \item{pt}{
positive threshold, class \code{numeric}.
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

rec <- recommendHPR(r)

}
