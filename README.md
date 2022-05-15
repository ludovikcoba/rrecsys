# \*\* THIS PROJECT IS NOT MAINTAINED ANYMORE \*\*

# rrecsys
[![CRAN](http://www.r-pkg.org/badges/version/rrecsys)](https://cran.r-project.org/web/packages/rrecsys/index.html)
[![CRAN](http://cranlogs.r-pkg.org/badges/rrecsys)](https://cran.r-project.org/web/packages/rrecsys/index.html)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/j0504s5kapv95xe5?svg=true)](https://ci.appveyor.com/project/ludovikcoba/rrecsys) 
[![Travis CI build status](https://travis-ci.org/ludovikcoba/rrecsys.svg?branch=master)](https://travis-ci.org/ludovikcoba/rrecsys)


A package for R that provides implementations of several state-of-the-art recommendation systems. 

Currently on rrecsys are developed the following non-personalized recommender systems (RS) algorithms:

* Global Average
* Item Average
* User Average
* Most popular

Collaborative filtering:

* Item Based K-nearest neighbors
* Simon Funk's SVD
* Bayesian Personalized Ranking (BPR)
* Weighted Alternated Least Squares (wALS)

rrecsys can be used to predict and recommend(top-N list) using any of the above algorithms. Algorithms work on both Likert scale and binary ratings but BPR and wALS are One-Class CF(OCCF) typical algorithms for implicit feedback. The package offers as well an evaluation methodology with the following standard metrics for the specific task:

* prediction: global or user based MAE and RMSE
* recommendation: precision, recall, F1, NDCG, rank score and all the elements of the confusion matrix.


## Installation & Loading the package
This section is dedicated to the recommender systems community with no experience in R. The package is available on CRAN and as well on [GitHub](https://github.com/ludovikcoba). Both versions are downloadable and installable. To install it from CRAN:

```R
install.packages("rrecsys")
```
Once the package is installed the usual thing to do would be to load it in the environment:
```R
library(rrecsys)
```
Usually we try to keep updated both GitHub and CRAN with the same version of the package. The version on GitHub is intended for the developing community and we recommend to use it with RStudio. On another vignette will be given details on how to extend rrecsys.

## Research

If you use our package in your research, please cite:


@inproceedings{Coba:2017:VAR:3109859.3109982,
 author = {\\c{C}oba, Ludovik and Symeonidis, Panagiotis and Zanker, Markus},
 title = {Visual Analysis of Recommendation Performance},
 booktitle = {Proceedings of the Eleventh ACM Conference on Recommender Systems},
 series = {RecSys '17},
 year = {2017},
 isbn = {978-1-4503-4652-8},
 location = {Como, Italy},
 pages = {362--363},
 numpages = {2},
 url = {http://doi.acm.org/10.1145/3109859.3109982},
 doi = {10.1145/3109859.3109982},
 acmid = {3109982},
 publisher = {ACM},
 address = {New York, NY, USA},
 keywords = {evaluation, r, recommendation algorithms, visualization},
} 
