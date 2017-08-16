\name{_ds-class}
\docType{class}
\alias{_ds-class}
\alias{_ds}
\alias{show,_ds-method}
\alias{summary,_ds-method}
\alias{rowAverages}
\alias{colAverages}
\alias{rowRatings}
\alias{colRatings}
\alias{numRatings}
\alias{sparsity}
\alias{sparsity,_ds-method}
\title{
Dataset class.
}
\description{
Defines a structure for a dataset that distinguishes between binary and non-binary feedback datasets. 
}
\section{Slots}{
  \describe{
    \item{\code{binary}:}{class \code{"logical"}, determines if the item dataset contains binary (i.e. 1/0) or non-binary ratings.}
    \item{\code{minimum}:}{class \code{"numeric"}, defines the minimal value present in the dataset.}
    \item{\code{maximum}:}{class \code{"numeric"}, defines the maximal value present in the dataset.}
    \item{\code{intScale}:}{object of class \code{"logical"}, if \bold{TRUE} the range of ratings in the dataset contains as well half star values.}
  }
}
\section{Methods}{
  \describe{
  \item{show}{signature(object = "_ds")}
  \item{sparsity}{signature(object = "_ds"): returns the sparsity of the dataset.}
  \item{summary}{signature(object = "_ds"): summary of the characteristics of the dataset.}
  }
}

