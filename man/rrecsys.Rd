\name{rrecsys}
\alias{rrecsys}
\alias{rrecsys,dataSet-method}
\alias{rrecsysRegistry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a recommender system.
}
\description{
Based on the specific given algorithm a recommendation model will be trained.  
}
\usage{
rrecsys(data, alg, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Training set of class \code{"matrix"}. The columns correspond to items and the rows correspond to users.
}
  \item{alg}{
A \code{"character"} string specifying the recommender algorithm to apply on the data.
}
  \item{\dots}{
other attributes, see \link[=rrecsys]{details}.
}
}
\details{
Based on the value of \emph{alg} the attributes will have different names and values.  Possible configuration of \code{alg} and it's meaning:

\enumerate{
\item \bold{itemAverage}. When \code{alg = "itemAverage"} the average rating of an item is used to make predictions and recommendations.

\item \bold{userAverage}. When \code{alg = "userAverage"} the average rating of a user is used to make predictions and recommendations.

\item \bold{globalAverage}. When \code{alg = "globalAverage"} the overall average of all ratings is used to make predictions and recommendations.

\item \bold{Mostpopular}. The most popular algorithm (\code{ alg = "mostpopular"}) is the most simple algorithm for recommendations. Item will be ordered based on the number of times that they were rated. Recommendations for a particular user will be the most popular items from the data set which are not contained in the user's training set.

\item \bold{IBKNN}.  As \code{ alg = "IBKNN"} a k-nearest neighbor item-based collaborative filtering algorithm.  It determines the "adjusted cosine" distance among the items as: \deqn{ sim(\vec{a}, \vec{b}) = \frac{\sum_{u \in U} (r_{u,a} - \overline{r_{u}}) \ast (r_{u,b} - \overline{r_{u}})}{\sqrt{(r_{u,a} - \overline{r_{u}})^2} \ast \sqrt{(r_{u,b} - \overline{r_{u}})^2}}}  The items \emph{a} and \emph{b} are considered as the corresponding rating vectors \eqn{\vec{a}} and \eqn{\vec{b}}.  It extracts, based on the value of the \emph{neigh} attribute, the number of closest neighbors for each item.  

\item \bold{FunkSVD}.  It implements \code{ alg = "funkSVD"}  a stochastic gradient descent optimization technique. The U(user) and V(item) factor matrices are initialized at small values and cropped to \emph{k} features.  Each feature is trained until \emph{convergence} (the convergence value has to be specified by the user).  On each loop the algorithm predicts \eqn{r'_{ui}} and calculates the error as: \deqn{r'_{ui} = u_{u} \ast v^{T}_{i}} \deqn{e_{ui} =r_{ui} - r'_{ui}} The factors are updated: \deqn{v_{ik} \gets v_{ik} + lambda \ast (e_{ui} \ast u_{uk} - gamma \ast v_{ik}) }
\deqn{u_{uk} \gets u_{uk} + lambda \ast (e_{ui} \ast v_{ik} - gamma \ast u_{uk}) }. The attribute \emph{lambda} represents the learning rate, while \emph{gamma} corresponds to the weight of the regularization term.

\item \bold{wALS}. The \code{alg = "wALS"} weighted Alternated Least squares method. For a given non-negative weight matrix \emph{W} the algorithm will perform updates on the item \emph{V} and user \emph{U} feature matrix as follows:
\deqn{
 U_i = R_i \ast \widetilde{W_i} \ast V \ast (V^T \ast \widetilde{W_i} \ast V + lambda (\sum_j W_{ij}) I ) ^{-1}
}
\deqn{
V_j = R_j^T \ast \widetilde{W_j} \ast U \ast (V^T \ast \widetilde{W_j} \ast u + lambda (\sum_i W_{ij}) I ) ^{-1} 
}
Initialy the \emph{V} matrix is initialized with Guassian random numbers with mean zero and small standard deviation. Than \emph{U} and \emph{V} are updated until \code{convergence}. The attribute \code{scheme} must specify the scheme(\code{uni, uo, io, co}) to use.


\item \bold{BPR}. In this implementation of BPR(\code{alg = "BPR"}) is applied a stochastic gradient descent approach that randomly choose triples from \eqn{D_R} and trains the model \eqn{\Theta}. In this implementation the BPR optimization criterion is applied on matrix factorization. If \eqn{R = U \times V^T}, where \emph{U} and \emph{V} are the usual feature matrix cropped to \emph{k} features, the parameter vector of the model is \eqn{\Theta = \langle U,V \rangle}. The boolean \code{randomInit} parameter determines whatever the feature matrix are initializated to a random value or at a static 0.1 value. The algorithm will use three regularization terms, \code{RegU} for the user features \emph{U}, \code{RegI} for positive updates and \code{RegJ} for negative updates of the item features \emph{V}, \code{lambda} is the learning rate, \code{autoConvergence} is a toggle to the auto convergence validation, \code{convergence} upper limit to the convergence, and \code{updateJ} if true updates negative item features.

}
To receive a list of all algorithms and their default configuration a call to \code{rrecsysRegistry} is advised.
}
\references{
D. Jannach, M. Zanker, A. Felfernig, and G. Friedrich. \emph{Recommender Systems: An Introduction}. Cambridge
University Press, New York, NY, USA, 1st edition, 2010. ISBN 978-0-521-49336-9.

Funk, S., 2006, \emph{Netflix Update: Try This at Home, \url{http://sifter.org/~simon/journal/20061211.html}}.

Y. Koren, R. Bell, and C. Volinsky. \emph{Matrix Factorization Techniques for Recommender Systems}. Computer, 42(8):30–37, Aug. 2009. ISSN 0018-9162. doi: 10.1109/MC.2009.263. \url{http://dx.doi.org/10.1109/MC.2009.263}. 

R. Pan, Y. Zhou, B. Cao, N. Liu, R. Lukose, M. Scholz, and Q. Yang. \emph{One-Class Collaborative Filtering}. In Data Mining, 2008. ICDM ’08. Eighth IEEE International Conference on, pages 502–511, Dec 2008. doi: 10.1109/ICDM.2008.16.

S. Rendle, C. Freudenthaler, Z. Gantner, and L. Schmidt-Thieme. \emph{BPR: Bayesian Personalized Ranking from Implicit Feedback}. In Proceedings of the Twenty-Fifth Conference on Uncertainty in Artificial Intelligence, UAI ’09, pages 452–461, Arlington, Virginia, United States, 2009. AUAI Press. ISBN 978-0-9749039-5-8. URL \url{http://dl.acm.org/citation.cfm?id=1795114.1795167}.
}

\value{
Depending on the \code{alg} value it will be either an object of type \code{\link{SVDclass}} or \code{\link{IBclass}}.
}

\examples{
myratings <- matrix(sample(c(0:5), size = 200, replace = TRUE, 
        prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

myratings <- defineData(myratings)

r <- rrecsys(myratings, alg = "funkSVD", k = 2)

r1 <- rrecsys(myratings, alg = "funkSVD", k = 3, 
              gamma = 0.01, lambda = 0.002)
              
r2 <- rrecsys(myratings, alg = "IBKNN", simFunct = 1, neigh = 5)

rrecsysRegistry$get_entries()


 }
