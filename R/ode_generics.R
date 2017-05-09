setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))
setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))
setGeneric("setState", function(object, x, vx, y, vy, ...) standardGeneric("setState"))
setGeneric("init", function(object, ...) standardGeneric("init"))


# setStepSize uses either of two step parameters: stepSize and dt
# stepSize works for most of the applications
# dt is used in Pendulum
setGeneric("setStepSize", function(object, stepSize, dt, ...) 
    standardGeneric("setStepSize"),
    signature = c("object", "stepSize", "dt"))

# setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))

setGeneric("getExactSolution", function(object, t, ...) 
    standardGeneric("getExactSolution"))
