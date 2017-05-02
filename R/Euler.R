source("./R/ODESolver.R")


setClass("Euler", slots = c( 
    rate = "numeric" 
),  
contains = c("ODESolver") 
)

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")
    return(.Object)
    # callNextMethod(.Object, ode)
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    object <- callNextMethod(object, stepSize = stepSize)  # call superclass init()
    object@rate <- object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object
})


setMethod("step", "Euler", function(object, ...) {
    # cat("Euler just called \n")
    # object@numEqn <- 4           # debugging why numEqn remains zero
    state <- getState(object@ode)
    rate  <- getRate(object@ode, state, object@rate)
    # cat("Euler:step:object@numEqn=", object@numEqn, "\n")
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
    }
    object@ode@state <- state       # return state and rate for new iter
    object@ode@rate  <- rate  
    object
}) 

setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})


setMethod("getStepSize", "Euler", function(object, ...) {
    object <- object
    return(object@stepSize)
})



# constructor
Euler <- function(.ode) {
    euler <- new("Euler", .ode)
    euler <- init(euler, euler@stepSize)
    return(euler)
}

