\name{dataChart}
\alias{dataChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of data characteristics.
}
\description{
This method visualizes data characteristics on a two dimensional graph, where "x" axes shows either items ordered by descending popularity, or users based on the number of ratings they have submitted. Moreover the "y" axes shows the number of ratings.
}

\usage{
dataChart(data, x = "items", y = "num_of_ratings")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{the dataset, class \code{"_ds"}.}
    
    \item{x}{class \code{"character"}, is the variable that will be shown on the "x" axis. Possible values are: \code{"items"}, \code{"users"}.}
    
    \item{y}{class \code{"character"}, is the variable that will be shown on the "y" axis. Possible values are: \code{"num_of_ratings"}, \code{"\%_of_ratings"}.}
    
    %\item{x_label}{class \code{"character"}, the label to be printed on the "x" axes.}
    
    %\item{y_label}{class \code{"character"}, the label to be printed on the "y" axes.}

}

\value{
Plot results.
}

\seealso{
See Also as \code{\link{_ds-class}}.
}
\examples{
data(mlLatest100k)

a <- defineData(mlLatest100k)

dataChart(a, x = "items", y = "num_of_ratings")

}