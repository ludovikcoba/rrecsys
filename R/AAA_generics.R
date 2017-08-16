setGeneric(name = "rrecsys", def = function(data, alg, ...) standardGeneric("rrecsys"))

setGeneric(name = "performSVD", def = function(data, k, learningRate, regCoef, biases, steps) standardGeneric("performSVD"))

setGeneric(name = "colRatings", def = function(x) standardGeneric("colRatings"))

setGeneric(name = "rowRatings", def = function(x) standardGeneric("rowRatings"))

setGeneric(name = "numRatings", def = function(x) standardGeneric("numRatings"))

setGeneric(name = "convert2DataSet", def = function(x) standardGeneric("convert2DataSet"))

setGeneric(name = "evalRec", def = function(model, ...) standardGeneric("evalRec"))

setGeneric(name = "evalTP", def = function(model, ...) standardGeneric("evalTP"))

setGeneric(name = "evalError", def = function(model, ...) standardGeneric("evalError"))

setGeneric(name = "evalPred", def = function(model, ...) standardGeneric("evalPred"))

setGeneric(name = "evalModel", def = function(data, folds) standardGeneric("evalModel"))

setGeneric(name = "getAUC", def = function(model, ...) standardGeneric("getAUC"))

setGeneric(name = "predict", def = function(model, ...) standardGeneric("predict")) 

setGeneric(name = "colAverages", def = function(x, ...) standardGeneric("colAverages"))

setGeneric(name = "rowAverages", def = function(x, ...) standardGeneric("rowAverages"))

setGeneric(name = "averageRating", def = function(x, ...) standardGeneric("averageRating"))

setGeneric(name = "results", def = function(object, ...) standardGeneric("results"))

setGeneric(name = "plot_TP_PopDist", def = function(model, ...) standardGeneric("plot_TP_PopDist"))

setGeneric(name = "plot_Error", def = function(model, ...) standardGeneric("plot_Error"))


# setGeneric(name = "", def = function() standardGeneric("")) 
