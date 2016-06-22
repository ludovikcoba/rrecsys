\name{nDCG}
\alias{nDCG}
\title{
Normalized Discounted Cumulative Gain
}
\description{
Metric for information retrival where positions are discounted logarithmically.
}
\usage{
nDCG(recommendedIDX, testSetIDX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recommendedIDX}{
indices of the recommended items. Object of class \code{numeric}.
}
  \item{testSetIDX}{
indices of the items in the test set. Object of class \code{numeric}
}
}
\details{
nDCG is computed as the ratio between Discounted Cumulative Gain(DCG) and idealized Discounted Cumulative Gain(IDCG): 
\deqn{DGC_{pos} = rel_1 + \sum_{i=2}^{pos} \frac{rel_i}{\log_2i}}
\deqn{IDGC_{pos} = rel_1 + \sum_{i=2}^{|h|-1} \frac{rel_i}{\log_2i}}
\deqn{nDCG_{pos} = \frac{DCG}{IDCG}}
}

\references{
Asela Gunawardana, Guy Shani, Evaluating Recommender Systems.
}

