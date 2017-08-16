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
%\usage{
%defineData(data, sparseMatrix = FALSE, binary = FALSE, minimum = 0.5, maximum = 5, intScale = FALSE, positiveThreshold = 3)
%}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{the dataset, class \code{"matrix"}.}
    
    \item{sparseMatrix}{class \code{"logical"}. If \code{FALSE} implies that the imput is a dense two dimensional matrix. If \code{TRUE} implies that the imput is arranges as coordinate list where entries are stored as list of (row, column, value) tuples.}
    
    \item{binary}{class \code{"logical"}, defines if the item dataset consists of binary (i.e. NA/1) or non-binary ratings. Default value \code{FALSE}.}
    
    \item{minimum}{class \code{"numeric"}, defines the minimal value present in the dataset. Default value \code{0.5}.}
    
    \item{maximum}{class \code{"numeric"}, defines the maximal value present in the dataset. Default value \code{5}.}
    
    \item{intScale}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.  Default value \code{FALSE}.}
    
    \item{positiveThreshold}{class \code{"numeric"}, in case \code{binary} is \code{TRUE}, \code{positiveThreshold} defines the threshold value for binarizing the dataset (i.e. any rating value >= \code{positiveThreshold} will be transformed to 1 and all other values to \code{NA}(corresponding to a not rated item).  Default value \code{0.5}.}
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

b <- defineData(mlLatest100k,binary = TRUE ,positiveThreshold = 3)

}
