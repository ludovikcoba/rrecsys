\name{evalModel}
\alias{evalModel}
\alias{evalModel,_ds-method}
\alias{evalModel,dataSet-method}
\alias{evalModel,sparseDataSet-method}
\title{
Creating the evaluation model.
}
\description{
Creates the dataset split for evaluation where ratings of each user are uniformly distributed over k random folds.  The function returns the list of items that are assigned to each fold, such that algorithms can be compared on the same train/test splits.  
}
\usage{
evalModel(data, folds)
}

\arguments{
  \item{data}{
dataset, of class \code{_ds}.
}
  \item{folds}{
The number of folds to use in the k-fold cross validation, of class \code{numeric}, default value set to 5.
}
}

\value{
An object of class \code{\link{evalModel-class}}. 
}
\seealso{
\code{\link{evalModel-class}}, \code{\link{evalRec}}, \code{\link{_ds}}.
}
\examples{

x <- matrix(sample(c(0:5), size = 200, replace = TRUE, 
     prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

d <- defineData(x)
     
my_2_folds <- evalModel(d, 2)             #output class evalModel.

my_2_folds
# 2 - fold cross validation model on the dataset with 20 users and 10 items.

my_2_folds@data                     #the dataset.
my_2_folds@folds                    #the number of folds in the model.
my_2_folds@fold_indices               #the index of each item in the fold.
     
}
