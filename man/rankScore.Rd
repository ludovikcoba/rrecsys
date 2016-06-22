\name{rankScore}
\alias{rankScore}
\title{
Rank Score
}
\description{
Rank Score extends the recall metric to take the positions of correct items in a ranked list into account.
}
\usage{
rankScore(recommendedIDX, testSetIDX, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recommendedIDX}{
indices of the recommended items. Object of class \code{numeric}.
}
  \item{testSetIDX}{
indices of the items in the test set. Object of class \code{numeric}
}
  \item{alpha}{
  is the ranking half life. Object of class \code{numeric}.
  }
}
\details{
Rank Score is defined as the ratio of the Rank Score of the correct items to best theoretical Rank Score achievable for the user:
\deqn{
rankscore_{p} =\sum_{i\in{h}} 2^{-\frac{rank(i)-1}{\alpha}}
}
\deqn{
rankscore_{max} =  \sum_{i=1}^{|T|} 2^{-\frac{i-1}{\alpha}}
}
\deqn{
rankscore = \frac{rankscore_p}{rankscore_{max}}
}
}


