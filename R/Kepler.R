#' Kepler.R
#' 
source("./R/ODE.R")


setClass("Kepler", slots = c(
    odeSolver = "Euler",
    GM = "numeric"
    ), 
    contains = c("ODE")
)

setMethod("initialize", "Kepler", function(.Object, ...) {
    .Object@GM <- 4 * pi * pi
    .Object@state <- vector("numeric", 5)
    .Object@odeSolver <- Euler(.Object)
    return(.Object)
})


setMethod("init", "Kepler", function(object, initState, ...) {
    object@state <- object@odeSolver@ode@state <- initState
    # callNextMethod(object@odeSolver, getStepSize(object@odeSolver))       
    # initialize providing the step size
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    # object@odeSolver <- callNextMethod(object, object@odeSolver@stepSize)   
    # object@odeSolver <- callNextMethod(object@odeSolver, getStepSize(object@odeSolver))  
    
    object@rate <- object@odeSolver@ode@rate
    object@state <- object@odeSolver@ode@state
    
    object
    
})

setMethod("getRate", "Kepler", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    r2 <- state[1] * state[1] + state[3] * state[3]
    r3 <- r2 * sqrt(r2)
    object@rate[1] <- state[2]
    object@rate[2] <- (- object@GM * state[1]) / r3
    object@rate[3] <- state[4]
    object@rate[4] <- (- object@GM * state[3]) / r3
    object@rate[5] <- 1
    
    object@state <- object@odeSolver@ode@state <- state
    object@odeSolver@ode@rate <- object@rate
    object@rate  
    
})

setMethod("getState", "Kepler", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

# constructor
Kepler <- function() {
    new("Kepler")
}
