\name{dataSet-class}
\docType{class}
\alias{dataSet-class}
\alias{dataSet}
\alias{nrow,dataSet-method}
\alias{ncol,dataSet-method}
\alias{show,dataSet-method}
\alias{dim,dataSet-method}
\alias{rowRatings}
\alias{rowRatings,dataSet-method}
\alias{colRatings}
\alias{colRatings,dataSet-method}
\alias{numRatings}
\alias{numRatings,dataSet-method}
\alias{sparsity}
\alias{sparsity,dataSet-method}
\alias{[,dataSet,ANY,ANY,missing-method}
\alias{coerce,dataSet,matrix-method}
\title{
Dataset class.
}
\description{
Defines a structure for a dataset that distinguishes between binary and non-binary feedback datasets.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{the dataset, class \code{"matrix"}.}
    \item{\code{binary}:}{class \code{"logical"}, determines if the item dataset contains binary (i.e. 1/0) or non-binary ratings.}
    \item{\code{minimum}:}{class \code{"numeric"}, defines the minimal value present in the dataset.}
    \item{\code{maximum}:}{class \code{"numeric"}, defines the maximal value present in the dataset.}
    \item{\code{halfStar}:}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.}
  }
}
\section{Methods}{
  \describe{
  \item{show}{signature(object = "dataSet")}
  \item{nrow}{signature(object = "dataSet"): number of rows of the dataset.}
  \item{ncol}{signature(object = "dataSet"): number of columns of the dataset.}
  \item{dim}{signature(object = "dataSet"): returns the dimensions of the dataset.}
  \item{rowRatings}{signature(object = "dataSet"): returns the number of ratings on each row.}
  \item{colRatings}{signature(object = "dataSet"): returns the number of ratings on each column.}
  \item{numRatings}{signature(object = "dataSet"): returns the total number of ratings.}
  \item{[}{signature(x = "dataSet", i = "ANY", j = "ANY", drop = "ANY")): returns a subset of the dataset.}
  \item{sparsity}{signature(object = "dataSet"): returns the sparsity of the dataset.}
  \item{coerce}{signature(from = "dataSet", to = "matrix")}
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
