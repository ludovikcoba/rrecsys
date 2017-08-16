\name{sparseDataSet-class}
\docType{class}
\alias{sparseDataSet-class}
\alias{sparseDataSet}
\alias{nrow,sparseDataSet-method}
\alias{ncol,sparseDataSet-method}
\alias{dim,sparseDataSet-method}
\alias{rowRatings,sparseDataSet-method}
\alias{colRatings,sparseDataSet-method}
\alias{numRatings,sparseDataSet-method}
\alias{[,sparseDataSet,ANY,ANY,missing-method}
\alias{coerce,sparseDataSet,matrix-method}
\alias{rowAverages,sparseDataSet-method}
\alias{colAverages,sparseDataSet-method}
\title{
Dataset class for tuples (user, item, rating).
}
\description{
Container for a sparse dataset that distinguishes between binary and non-binary feedback datasets. Data are stored as tuples (user, item, rating). Extends \code{\link{_ds}}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{the dataset, class \code{"matrix"}.}
    \item{\code{binary}:}{class \code{"logical"}, determines if the item dataset contains binary (i.e. 1/0) or non-binary ratings.}
    \item{\code{minimum}:}{class \code{"numeric"}, defines the minimal value present in the dataset.}
    \item{\code{maximum}:}{class \code{"numeric"}, defines the maximal value present in the dataset.}
    \item{\code{intScale}:}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.}
    \item{\code{userID}:}{class \code{"numeric"}, array containing all user IDs.}
    \item{\code{itemID}:}{class \code{"numeric"}, array containing all item IDs.}
    \item{\code{userPointers}:}{class \code{"list"}, pointer to all users position in the dataset.}
    \item{\code{itemPointers}:}{class \code{"list"}, pointer to all items position in the dataset.}
  }
}
\section{Methods}{
  \describe{
  \item{nrow}{signature(object = "sparseDataSet"): number of rows of the dataset.}
  \item{ncol}{signature(object = "sparseDataSet"): number of columns of the dataset.}
  \item{dim}{signature(object = "sparseDataSet"): returns the dimensions of the dataset.}
  \item{rowRatings}{signature(object = "sparseDataSet"): returns the number of ratings on each row.}
  \item{colRatings}{signature(object = "sparseDataSet"): returns the number of ratings on each column.}
  \item{numRatings}{signature(object = "sparseDataSet"): returns the total number of ratings.}
  \item{[}{signature(x = "sparseDataSet", i = "ANY", j = "ANY", drop = "ANY")): returns a subset of the dataset.}
  \item{coerce}{signature(from = "sparseDataSet", to = "matrix")}
  \item{rowAverages}{signature(object = "sparseDataSet"): returns the average rating on each row.}
  \item{colAverages}{signature(object = "sparseDataSet"): returns the average rating on each column.}
  }
}

