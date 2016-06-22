\name{mlLatest100k}
\alias{mlLatest100k}
\docType{data}
\title{
Movielens Latest
}
\description{

This dataset (ml-latest-small) is a 5-star rating dataset from [MovieLens](http://movielens.org), a movie recommendation service of the GroupLens research group at the University of Minnesota. It contains 100234 ratings  across 8927 movies. The data was created by 718 users between March 26, 1996 and August 05, 2015. This dataset was generated on August 06, 2015.
Users were selected at random for inclusion. All selected users had rated at least 20 movies. 
The data is edited and structured as a matrix and distributed as such. Below the usage license of this redistributed data is cited below.


}
\usage{data("mlLatest100k")}
\format{
  The format is:
 num [1:718, 1:8915] 5 3 0 0 4 4 0 3 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:718] "1" "2" "3" "4" ...
  ..$ : chr [1:8915] "Toy Story (1995)" "Jumanji (1995)" "GoldenEye (1995)" "Twelve Monkeys (a.k.a. 12 Monkeys) (1995)" ...
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\section{Usage  License}{
Neither the University of Minnesota nor any of the researchers involved can guarantee the correctness of the data, its suitability for any particular purpose, or the validity of results based on the use of the data set. The data set may be used for any research purposes under the following conditions:
\enumerate{
\item The user may not state or imply any endorsement from the University of Minnesota or the GroupLens Research Group.
\item  The user must acknowledge the use of the data set in publications resulting from the use of the data set, and must send us an electronic or paper copy of those publications.
\item  The user may redistribute the data set, including transformations, so long as it is distributed under these same license conditions.
\item  The user may not use this information for any commercial or revenue-bearing purposes without first obtaining permission from a faculty member of the GroupLens Research Project at the University of Minnesota.
\item  The executable software scripts are provided "as is" without warranty of any kind, either expressed or implied, including, but not limited to, the implied warranties of merchantability and fitness for a particular purpose. The entire risk as to the quality and performance of them is with you. Should the program prove defective, you assume the cost of all necessary servicing, repair or correction.
}
In no event shall the University of Minnesota, its affiliates or employees be liable to you for any damages arising out of the use or inability to use these programs (including but not limited to loss of data or data being rendered inaccurate).
If you have any further questions or comments, please email \email{grouplens-info@cs.umn.edu}.
}

\source{
\url{http://grouplens.org/datasets/movielens/latest/}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}

\keyword{datasets}
