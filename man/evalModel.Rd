\name{evalModel}
\alias{evalModel}
\alias{evalModel,dataSet-method}
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
dataset, of class \code{dataSet}.
}
  \item{folds}{
The number of folds to use in the k-fold cross validation, of class \code{numeric}, default value set to 5.
}
}

\value{
An object of class \code{\link{evalModel-class}}. 
}
\seealso{
\code{\link{evalModel-class}}, \code{\link{evalRec}}, \code{\link{dataSet}}.
}
\examples{

x <- matrix(sample(c(0:5), size = 200, replace = TRUE, 
     prob = c(.6,.08,.08,.08,.08,.08)), nrow = 20, byrow = TRUE)

x <- defineData(x)
     
my_10_folds <- evalModel(x, 10)             #output class evalModel.

my_6_folds <- evalModel(x, 6)  

my_6_folds
#6 - fold cross validation model on the dataset with 20 users and 10 items.

my_6_folds@data                     #the dataset.
my_6_folds@folds                    #the number of folds in the model.
my_6_folds@fold_indices               #the index of each item in the fold.
     
}
