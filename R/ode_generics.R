
# setGeneric("init", function(object, initState, ...) standardGeneric("init"))
setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))
setGeneric("getStepSize", function(object, time, ...) standardGeneric("getStepSize"))
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))
setGeneric("init", function(object, stepSize, ...) standardGeneric("init"))