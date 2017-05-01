# source("./R/ode_generics.R")
# source("./R/ODESolver.R")
source("./R/ODE.R")


setClass("Euler", slots = c(
    stepSize = "numeric",
    numEqn   = "numeric",
    ode      = "ODE"
), prototype = prototype(
    stepSize = 0.1,
    numEqn = 0
)
)

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    # .Object <- callNextMethod(.Object, ode)
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")
    return(.Object)
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    # cat("Euler:init \n")
    # object <- initSolver(object, stepSize)
    object@stepSize <- stepSize
    state <- getState(object@ode)
    object@ode@state <- state
    
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        # cat("assigning a value > 0 \n")
        object@numEqn = length(state)
        # cat("ODESOlver:object@numEqn =", object@numEqn, "\n")
    }
    object@ode@rate <- vector("numeric", object@numEqn)
    object
})


setMethod("step", "Euler", function(object, ...) {
    # who calls `step()`? : it is called from the main application, from a loop
    state <- getState(object@ode)
    # cat("Euler:step:rate:", object@rate, "\n")
    cat("Euler:step:state:", state, "\n")
    # cat("object@stepSize:", object@stepSize, "\n")
    # rate  <- getRate(object@ode, object@ode@state, object@rate)
    
    rate  <- getRate(object@ode, state, object@rate)
    cat("Euler:step:rate:", object@ode@rate, "\n")
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
        # cat(i, state[i], rate[i], "\n")
    }
    cat("Euler:step:state:", state, "\n")
    object@ode@state <- state
    object@ode@rate  <- rate  # does rate has to return?
    object
}) 

setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    object@stepSize = stepSize
    # cat("step size is ", object@stepSize, "\n")
    object
})


setMethod("getStepSize", "Euler", function(object, ...) {
    object <- object
    return(object@stepSize)
})


# constructor
Euler <- function(.ode) {
    euler <- new("Euler", .ode)
    # cat("euler@stepSize:", euler@stepSize, "\n")
    euler <- init(euler, euler@stepSize)
    return(euler)
}

