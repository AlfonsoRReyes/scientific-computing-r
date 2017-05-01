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
    # .Object <- callNextMethod()
    .Object@state <- vector("numeric", 3)
    return(.Object)
})


setMethod("getState", "FallingParticleODE", function(object, ...) {
    # Gets the state variables.
    # cat("getState() called with ", class(object), "\n")
    return(object@state)
})

setMethod("getRate", "FallingParticleODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    # cat("getRate()  called with ", class(object), "\n")
    object@rate[1] <- state[2]
    object@rate[2] <- - object@g
    object@rate[3] <- 1
    object@rate   # last change
    
    # rate[1] <-   state[2]
    # rate[2] <- - object@g
    # rate[3] <-   1
    # 
    # # object@rate <- rate
    # # object@state <- state
    # 
    # return(rate)   # last change
    
})



# constructor
FallingParticleODE <- function(y, v) {
    .FallingParticleODE <- new("FallingParticleODE", y, v)
    # .FallingParticleODE@state <- vector("numeric", 3)
    .FallingParticleODE@state[1] <- y
    .FallingParticleODE@state[2] <- v
    .FallingParticleODE@state[3] <- 0
    .FallingParticleODE
}