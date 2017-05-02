setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))

setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))

setGeneric("doStep", function(object, ...) standardGeneric("doStep"))
setGeneric("init", function(object, ...) standardGeneric("init"))
# setGeneric("init", function(object, stepSize, ...) standardGeneric("init"))