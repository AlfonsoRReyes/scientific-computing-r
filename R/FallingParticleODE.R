# FallingParticleODE
source("./R/ODE.R")

setClass("FallingParticleODE", slots = c(
    g = "numeric"
    ), 
    prototype = prototype(
        g = 9.8
    ),
contains = c("ODE")
)


setMethod("initialize", "FallingParticleODE", function(.Object, y, v, ...) {
    .Object@state <- vector("numeric", 3)
    .Object@state[1]  <- y
    .Object@state[2]  <- v
    .Object@state[3]  <- 0
    return(.Object)
})


setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    # cat("getState() called with ", class(object), "\n")
    return(object@state)
})

setMethod("getRate", "ODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    # cat("getRate()  called with ", class(object), "\n")
    object@rate[1] <- state[2]
    object@rate[2] <- - object@g
    object@rate[3] <- 1
    object
})






FallingParticleODE <- function(y, v) {
    # state <- vector("numeric", 3)
    # state[1] <- y
    # state[2] <- v
    # state[3] <- 0
    new("FallingParticleODE", y, v)
}