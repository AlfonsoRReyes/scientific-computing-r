source("./R/ODE.R")


setClass("ODESolver", slots = c(
    stepSize = "numeric",
    numEqn   = "numeric",
    ode      = "ODE"
), prototype = prototype(
    stepSize = 0.1,
    numEqn = 0
)
)

setMethod("initialize", "ODESolver", function(.Object, .ode, ...) {
    .Object <- init(.Object, 0.1)                                 # diference #1
    return(.Object)
})

setMethod("step", "ODESolver", function(object, ...) {
    # object
})

setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})

setMethod("init", "ODESolver", function(object, stepSize, ...) {
    # cat("ODESolver:init\n")
    object@stepSize <- stepSize
    state <- getState(object@ode)
    # object@ode@state <- state      # removing this does not have effect
    # cat("ODESolver:init:state", state, "\n")
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        object@numEqn = length(state)
    }
    object
})

setMethod("getStepSize", "ODESolver", function(object, ...) {
    return(object@stepSize)
})


# constructor
ODESolver <- function(.ode) {
    odesolver <- new("ODESolver", .ode)
    odesolver@ode <- .ode
    # odesolver <- init(odesolver, 0.1)
    odesolver
}