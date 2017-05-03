source("./R/ODESolver.R")

# /**
# * Euler implements an Euler method ODE solver.
# *
# * The Euler method is unstable for many systems.  It is included as an  
# * example of how to use the ODE and ODESolver interface.
# *
# * @author              Wolfgang Christian
# * @version 1.0
# * converted to R by    Alfonso R> Reyes
# */

setClass("Euler", 
    contains = c("ODESolver") 
)

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")
    return(.Object)
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    object <- callNextMethod(object, stepSize) # call superclass init  #
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object
})


setMethod("step", "Euler", function(object, ...) {
    state <- getState(object@ode)
    rate  <- getRate(object@ode, state, object@ode@rate)             
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
    }
    object@ode@state <- state       # return state and rate for new iter
    object@ode@rate  <- rate  
    object
}) 

setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    object@stepSize <-  stepSize
    object
})


setMethod("getStepSize", "Euler", function(object, ...) {
    return(object@stepSize)
})



# constructor
Euler <- function(.ode) {
    euler <- new("Euler", .ode)
    euler <- init(euler, euler@stepSize)                         
    return(euler)
}

