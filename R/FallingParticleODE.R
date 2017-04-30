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


setMethod("initialize", "FallingParticleODE", function(.Object, ...) {
    .Object@state <- vector("numeric", 3)
    # .Object@state[1]  <- y
    # .Object@state[2]  <- v
    # .Object@state[3]  <- 0
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



# constructor
FallingParticleODE <- function(y, v) {
    .FallingParticleODE <- new("FallingParticleODE", y, v)
    .FallingParticleODE@state <- vector("numeric", 3)
    .FallingParticleODE@state[1] <- y
    .FallingParticleODE@state[2] <- v
    .FallingParticleODE@state[3] <- 0
    .FallingParticleODE
}