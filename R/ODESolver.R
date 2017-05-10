# ODESolver.R
#


setClass("ODESolver")


setMethod("init", "ODESolver", function(object, stepSize, ...) {
    object
})

setMethod("step", "ODESolver", function(object, ...) {
    # object
})

setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    object
})

setMethod("getStepSize", "ODESolver", function(object, ...) {
    return(object@stepSize)
})
