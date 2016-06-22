\name{defineData}
\alias{defineData}
\alias{defineData,matrix-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Define dataset.
}
\description{
Defines your dataset, if either it is implicit or explicit. 
}
\usage{
defineData(data, binary = FALSE, minimum = 1, maximum = 5, 
  halfStar = FALSE, goodRating = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{the dataset, class \code{"matrix"}.}
    
    \item{binary}{class \code{"logical"}, defines if the item dataset consists of binary (i.e. 0/1) or non-binary ratings. Deafault value \code{FALSE}.}
    
    \item{minimum}{class \code{"numeric"}, defines the minimal value present in the dataset. Deafault value \code{1}.}
    
    \item{maximum}{class \code{"numeric"}, defines the maximal value present in the dataset. Deafault value \code{5}.}
    
    \item{halfStar}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.  Deafault value \code{FALSE}.}
    
    \item{goodRating}{class \code{"numeric"}, in case \code{binary} is \code{TRUE}, \code{goodRating} defines the threshold value for binarizing the dataset (i.e. any rating value >= \code{goodRating} will be transformed to 1 and all other values to 0(corresponding to a not rated item).  Deafault value \code{0.5}.}
}

\value{
Returns an object of class \code{"dataSet"}.
}

\seealso{
See Also as \code{\link{dataSet-class}}.
}
\examples{
data(mlLatest100k)

a <- defineData(mlLatest100k)

b <- defineData(mlLatest100k, TRUE, goodRating = 3)

}
