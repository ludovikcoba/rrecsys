\name{evalModel-class}
\docType{class}
\alias{evalModel-class}
\alias{show,evalModel-method}
\title{
Evaluation model.
}
\description{
Class that contains the data and a distribution of the uniform distribution of ratings onto k-folds.  
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{the dataset, class \code{"matrix"}.}
    \item{\code{folds}:}{number of k - folds, class \code{"numeric"}.}
    \item{\code{fold_indices}:}{a list with k slots, each slot represents a fold and contains the index of items assigned to that fold, class \code{"list"}.}
    \item{\code{fold_indices_x_user}:}{a list that specifies specifically for each user the distribution of the items in the folds, class \code{"list"}.}
    }
}
\section{Methods}{
  \describe{
  \item{\code{show}}{signature(object = "evalModel")}
  }
}
\details{
The fold_indices list contains the indexes to access the dataset on one dimension.  A matrix can be addressed as a one dimensional array, considered as an extension of each column after another.  E.g: in a matrix M with 10 rows and 20 columns, M[10] == M[10, 1]; M[12] == M[2,2].  
}