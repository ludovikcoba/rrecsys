\name{evalRecResults}
\docType{class}
\alias{evalRecResults}
\alias{evalRecResults-class}
\alias{show,evalRecResults-method}
\alias{results}
\alias{results,evalRecResults-method}
\title{
  Evaluation results.
}
\description{
  Defines a structure for the results obtained by evaluating an algorithm
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{class \code{"_ds"}, the dataset.}
    \item{\code{alg}:}{class \code{"character"}, the name of the used algorithm.}
    \item{\code{topN}:}{class \code{"numeric"}, the number N of Top-N items recommended to each user.}
    \item{\code{topNGen}:}{class \code{"character"}, the name of the  recommendation algorithm.}
    \item{\code{positiveThreshold}:}{class \code{"numeric"}, indicating the threshold of the ratings to be considered a good. This attribute is not used when evaluating implicit feedback.}
    \item{\code{alpha}:}{class \code{numeric}, is the half-life parameter for the rankscore metric.}
    \item{\code{parameters}:}{class \code{"list"}, parameters used in the configuration of the algorithm.}
    \item{\code{TP}:}{class \code{"numeric"}, True Positives count on each fold.}
    \item{\code{FP}:}{class \code{"numeric"}, False Positives count on each fold.}
    \item{\code{TN}:}{class \code{"numeric"}, True Negatives count on each fold.}
    \item{\code{FN}:}{class \code{"numeric"}, False Negatives count on each fold.}
    \item{\code{precision}:}{class \code{"numeric"}, precision measured on each fold. }
    \item{\code{recall}:}{class \code{"numeric"}, recall measured on each fold.}
    \item{\code{F1}:}{class \code{"numeric"}, F1 measured on each fold.}
    \item{\code{nDCG}:}{class \code{"numeric"}, nDCG measured on each fold.}
    \item{\code{rankscore}:}{class \code{"numeric"}, rankscore measured on each fold.}
    \item{\code{item_coverage}:}{class \code{"numeric"}, item coverage.}
    \item{\code{user_coverage}:}{class \code{"numeric"}, user coverage.}
    \item{\code{ex.time}:}{class \code{"numeric"}, the execution time.}
    \item{\code{TP_count}:}{class \code{"numeric"}, True positives count on each item.}
    \item{\code{rec_counts}:}{class \code{"numeric"}, counts how many times an item was recommended.}
    \item{\code{rec_popularity}:}{class \code{"numeric"}, popularity of recommendations.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{show}}{signature(object = "evalRecResults")}
    \item{\code{results}}{signature(object = "evalRecResults", metrics = "character"): returns a subset of the results based on the required metric.}
    }
}
                    