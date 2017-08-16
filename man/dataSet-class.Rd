\name{dataSet-class}
\docType{class}
\alias{dataSet-class}
\alias{dataSet}
\alias{nrow,dataSet-method}
\alias{ncol,dataSet-method}
\alias{dim,dataSet-method}
\alias{rowRatings,dataSet-method}
\alias{colRatings,dataSet-method}
\alias{numRatings,dataSet-method}
\alias{[,dataSet,ANY,ANY,missing-method}
\alias{coerce,dataSet,matrix-method}
\alias{rowAverages,dataSet-method}
\alias{colAverages,dataSet-method}
\title{
Dataset class.
}
\description{
Container for a dense dataset that distinguishes between binary and non-binary feedback datasets. Extends \code{\link{_ds}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{the dataset, class \code{"matrix"}.}
    \item{\code{binary}:}{class \code{"logical"}, determines if the item dataset contains binary (i.e. 1/0) or non-binary ratings.}
    \item{\code{minimum}:}{class \code{"numeric"}, defines the minimal value present in the dataset.}
    \item{\code{maximum}:}{class \code{"numeric"}, defines the maximal value present in the dataset.}
    \item{\code{intScale}:}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.}
  }
}
\section{Methods}{
  \describe{
  \item{nrow}{signature(object = "dataSet"): number of rows of the dataset.}
  \item{ncol}{signature(object = "dataSet"): number of columns of the dataset.}
  \item{dim}{signature(object = "dataSet"): returns the dimensions of the dataset.}
  \item{rowRatings}{signature(object = "dataSet"): returns the number of ratings on each row.}
  \item{colRatings}{signature(object = "dataSet"): returns the number of ratings on each column.}
  \item{numRatings}{signature(object = "dataSet"): returns the total number of ratings.}
  \item{[}{signature(x = "dataSet", i = "ANY", j = "ANY", drop = "ANY")): returns a subset of the dataset.}
  \item{coerce}{signature(from = "dataSet", to = "matrix")}
  \item{rowAverages}{signature(object = "dataSet"): returns the average rating on each row.}
  \item{colAverages}{signature(object = "dataSet"): returns the average rating on each column.}
  }
}
\examples{
x <- matrix(sample(c(0:5), size = 100, replace = TRUE, 
  prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

x <- defineData(x)

colRatings(x)

rowRatings(x)

numRatings(x)

sparsity(x)

a <- x[1:10,2:3]


}
