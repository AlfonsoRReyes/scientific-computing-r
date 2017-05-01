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
    # cat("initializing class: ", class(.Object), "\n")
    # .Object@stepSize <- 0.1
    # .Object@numEqn <- 0
    # .Object <- callNextMethod(.Object)
    # .Object@ode <- .ode
    # .Object <- initSolver(.Object, 0.1)
    # callNextMethod(.Object)
    return(.Object)
})

setMethod("step", "ODESolver", function(object, ...) {
})

# setMethod("init", "ODESolver", function(object, stepSize, ...) {
    # replace by initSOlver below
#     object@stepSize <- stepSize
#     object@ode@state <- getState(object@ode)
#     
# })

setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    object@stepSize = stepSize
    # cat("step size is ", object@stepSize, "\n")
    object
})

setMethod("initSolver", "ODESolver", function(object, stepSize, ...) {
    # this method is not in the original Java
    object@stepSize <- stepSize
    state <- getState(object@ode)
    object@ode@state <- state
    # cat("ODESOlver:length(state) =", length(state), "\n")

    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        # cat("assigning a value > 0 \n")
        object@numEqn = length(state)
        # cat("ODESOlver:object@numEqn =", object@numEqn, "\n")
    }
    # object@ode@state <- state
    object
})

setMethod("getStepSize", "ODESolver", function(object, ...) {
    return(object@stepSize)
})


# constructor
ODESolver <- function(.ode) {
    odesolver <- new("ODESolver", .ode)
    odesolver@ode <- .ode
    odesolver <- initSolver(odesolver, 0.1)
    odesolver
}