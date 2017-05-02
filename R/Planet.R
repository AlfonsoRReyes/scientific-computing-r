source("./R/ODE.R")
source("./R/Euler.R")


setClass("Planet", slots = c(
    odeSolver = "Euler",
    GM = "numeric"
    ), 
    contains = c("ODE")
)

setMethod("initialize", "Planet", function(.Object, ...) {
    .Object@GM <- 4 * sqrt(pi * pi)
    .Object@state <- vector("numeric", 5)
    ode <- new("ODE")
    .Object@odeSolver <- new("Euler", ode)
    cat(".Object@odeSolver:numEqn", .Object@odeSolver@numEqn, "\n")
    return(.Object)
    # callNextMethod(.Object, .Object@ode)
    # callNextMethod(.Object)
})

setMethod("doStep", "Planet", function(object, ...) {
    # Gets the state variables.
    # cat("doStep() called with ", class(object), "\n")
    # object@odeSolver@stepSize <- step(object@odeSolver)
    object@odeSolver <- step(object@odeSolver)
    # cat(object@state[1], object@state[2])
    object
})

setMethod("init", "Planet", function(object, initState, ...) {
    cat("Planet:init:initState", initState, "\n")
    object@state <- initState
    cat("Planet:init:state =", object@state, "\n")
    cat("Planet:init:len:state =", length(object@state), "\n")
    # object <- callNextMethod(object, stepSize)        # call superclass init()
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    object
})

setMethod("getRate", "Planet", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    # cat("getRate()  called with ", class(object), "\n")
    r2 <- state[1] * state[1] + state[3] * state[3]
    r3 <- r2 * sqrt(r2)
    object@rate[1] <- state[2]
    object@rate[2] <- (- object@GM * state[1]) / r3
    object@rate[3] <- state[4]
    object@rate[4] <- (- object@GM * state[3]) / r3
    object@rate[5] <- 1
    
    object@state <- state
    object
})

setMethod("getState", "Planet", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})
