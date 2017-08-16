\name{evalChart}
\alias{evalChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of data characteristics.
}
\description{
This method visualizes data characteristics on a two dimensional graph, where "x" axes shows either items ordered by descending popularity, or users based on the number of ratings they have submitted. Moreover the "y" axes shows the number of ratings.
}

\usage{
evalChart(res, x = "items", y = "TP", x_label, y_label, y_lim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{res}{evaluation results, class \code{"evalRecResults"}.}
    
    \item{x}{class \code{"character"}, is the variable that will be shown on the "x" axis. Possible values are: \code{"items"}, \code{"users"}.}
    
    \item{y}{class \code{"character"}, is the variable that will be shown on the "y" axis. Possible values are: \code{"num_of_ratings"}, \code{"\%_of_ratings"}.}
    
    \item{x_label}{class \code{"character"}, the label to be printed on the "x" axes.}
    
    \item{y_label}{class \code{"character"}, the label to be printed on the "y" axes.}
    
    \item{y_lim}{class \code{"numeric"}, scale of the "y" axes.}

}

\value{
Plot results.
}

\seealso{
See Also as \code{\link{evalRecResults-class}}.
}
