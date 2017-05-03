setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))

setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))

setGeneric("doStep", function(object, ...) standardGeneric("doStep"))
setGeneric("setState", function(object, x, vx, y, vy, ...) standardGeneric("setState"))

setGeneric("init", function(object, ...) standardGeneric("init"))