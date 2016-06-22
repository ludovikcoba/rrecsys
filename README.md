# rrecsys 
[![CRAN](http://www.r-pkg.org/badges/version/rrecsys)](https://cran.r-project.org/web/packages/rrecsys/index.html)
[![CRAN](http://cranlogs.r-pkg.org/badges/rrecsys)](https://cran.r-project.org/web/packages/rrecsys/index.html)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/j0504s5kapv95xe5?svg=true)](https://ci.appveyor.com/project/ludovikcoba/rrecsys)
[![Travis CI build status](https://travis-ci.org/ludovikcoba/rrecsys.svg?branch=master)](https://travis-ci.org/ludovikcoba/rrecsys)

A package for R that provides implementations of several popular recommendation systems. They can process standard recommendation datasets (user/item matrix) as input and generate rating predictions and recommendation lists.  Standard algorithm implementations included in this package are: Global/Item/User-Average baselines, Item-Based KNN, FunkSVD, BPR and weighted ALS. They can be assessed according to the standard offline evaluation methodology for recommender systems using measures such as MAE, RMSE, Precision, Recall, AUC, NDCG, RankScore and coverage measures. The package is intended for rapid prototyping of recommendation algorithms and education purposes. 

## Installation & Loading

**rrecsys** can be installed on the R console:
```R
install.packages("rrecsys")
```

To load **rrecsys**:
```R
library(rrecsys)
```

# Example
The package is equipped with Movilens Latest dataset. To load the dataset in the environment:
```R
data("mlLatest100k")

View(mlLatest100k)
```
An important step is to imbue the dataset with the necessary information for the RS to process it. In rrecsys:
```R
mlL <- defineData(mlLatest100k, minimum = 0.5, halfStar = T)

mlL

binML <- defineData(mlLatest100k, binray = TRUE, goodRating = 0.5)
```
The goodRating attribute defines a threshold for binarizing the dataset.
The dataset can be explored and manipulated as follows:
```R
nr_ratings_users <- rowRatings(mlL)
nr_ratings_items <- colRatings(mlL)
numRatings(mlL)
sparsity(mlL)
#plot of the long tail distribution of the popularity
plot(sort(nr_ratings_items, decreasing = T))
# cropping mlLatest 
smallML <- mlL[rowRatings(mlL) >= 55, colRatings(mlL) >= 45]
sparsity(smallML)
smallML
sparsity(binML)
binML <- binML[rowRatings(binML) >= 40, colRatings(binML) >= 40]
binML
```

The package is based on a regystry. A user may access the registry to view the available RS algorithms and their default vales.
```R
rrecsysRegistry
```

The main function of this package is rrecsys, which trains a model for the given algorithm. Given a certain rating matrix let’s suppose we want to train a model with rrecsys:
```R
a <- c(5, 3, 4, 4, 0, 3, 1, 2, 3, 3, 4, 3, 4, 3, 5, 3, 3, 1, 5, 4, 1, 5, 5, 2, 1)
d <- matrix(a, nrow = 5, byrow = T)
rownames(d) <- paste0("user", 1:5)
colnames(d) <- paste0("item", 1:5)
rownames(d)[1] <- "Alice"
View(d)
#given the above rating matrix we define it to process through rrecsys
d <- defineData(d)
#train IB k-NN
ibr1 <- rrecsys(d, "ibknn", neigh = 2)
# rrecsys returns on object specific for the required algorithms. Specific elements of the algorithm will be returned as slots.
ibr1@sim
ibr1@sim_index_kNN
# train FunkSVD model
svdr1 <- rrecsys(d, "funk", k = 2)
#accessing features:
svdr1@factors

# train a model on global, item, user averages for smallML
gloAv <- rrecsys(smallML, "global")
itemAv <- rrecsys(smallML, "item")
userAv <- rrecsys(smallML, "user")

View(gloAv@average)

View(itemAv@average)

View(userAv@average)

# item based training
ibk5 <- rrecsys(smallML, "ibk", neigh = 5)
ibk10 <- rrecsys(smallML, "ibk", neigh = 10)

View(ibk5@sim_index_kNN)
View(ibk10@sim_index_kNN)

# FunkSVD

funk10 <- rrecsys(smallML, "funk", k = 10)

View(funk10@factors$U)

View(funk10@factors$V)

# predicting
# coercion of dataSet object to matrix
smallmat <- as(smallML, "matrix")
View(smallmat)

pglAv <- predict(gloAv)
View(pglAv)

pitemAv <- predict(itemAv)
View(pitemAv)

puserAv <- predict(userAv)

View(puserAv)

pib5 <- predict(ibk5)
pib10 <- predict(ibk10)


pfunk <- predict(funk10)

# recommending

recuseAv <- recommend(itemAv, topN = 2)

recuseAv

recFunk <- recommend(funk10)

recFunk@recommended[[1]]

# bpr, wals

binBPR <- rrecsys(binML, "bpr")

binwALS <- rrecsys(binML, "wals", scheme = "uni")
```
