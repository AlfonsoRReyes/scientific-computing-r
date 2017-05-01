source("./R/ODE.R")
source("./R/Euler.R")


setClass("Planet", slots = c(
    odeSolver = "Euler",
    GM = "numeric"
    ), 
    contains = c("ODE")
)

setMethod("initialize", "Planet", function(.Object, ...) {
    # cat("initializing class: ", class(.Object), "\n")
    .Object@GM <- 4 * sqrt(pi * pi)
    .Object@state <- vector("numeric", 5)
    ode <- new("ODE")
    .Object@odeSolver <- new("Euler", ode)
    # return(.Object)
    # callNextMethod(.Object, .Object@ode)
    callNextMethod(.Object)
})

setMethod("doStep", "Planet", function(object, ...) {
    # Gets the state variables.
    # cat("doStep() called with ", class(object), "\n")
    # object@odeSolver@stepSize <- step(object@odeSolver)
    step(object@odeSolver)
    # cat(object@state[1], object@state[2])
    object
})

setMethod("init", "Planet", function(object, initState, ...) {
    # cat("initState", initState, "\n")
    object@state <- initState
    cat("state@init planet", object@state, "\n")
    object@odeSolver <- initSolver(object@odeSolver, getStepSize(object@odeSolver))
    object
})

setMethod("getRate", "Planet", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    cat("getRate()  called with ", class(object), "\n")
    r2 <- state[1] * state[1] + state[3] * state[3]
    r3 <- r2 * sqrt(r2)
    object@rate[1] <- state[2]
    object@rate[2] <- state[1] / r3
    object@rate[3] <- state[4]
    object@rate[4] <- state[3] / r3
    object@rate[5] <- 1
    object
})

setMethod("getState", "Planet", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})
