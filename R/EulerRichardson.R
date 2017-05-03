source("./R/ODESolver.R")

# * An Euler-Richardson (midpoint) method ODE solver.
# *
# * The Euler-Richardson method uses the state at the beginning of the interval
# * to estimate the state at the midpoint.
# *
#     * x(midpoint) = x(n) + v(n)*dt/2
#     * v(midpoint) = v(n) + a(n)*dt/2
#     * t(midpoint) = t(n) + dt/2
#     *
#     * The midpoint state is then used to calculate the final state.
#     * @author       Wolfgang Christian


setClass("EulerRichardson", 
    contains = c("ODESolver") 
)

setMethod("initialize", "EulerRichardson", function(.Object, ode, ...) {
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")
    return(.Object)
})


setMethod("init", "EulerRichardson", function(object, stepSize, ...) {
    object <- callNextMethod(object, stepSize) # call superclass init  # diff 21
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object
})


setMethod("step", "EulerRichardson", function(object, ...) {
    state <- getState(object@ode)
    rate  <- getRate(object@ode, state, object@ode@rate)             # diff 4
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
    }
    object@ode@state <- state       # return state and rate for new iter
    object@ode@rate  <- rate  
    object
}) 

setMethod("setStepSize", "EulerRichardson", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})


setMethod("getStepSize", "EulerRichardson", function(object, ...) {
    return(object@stepSize)
})



# constructor
EulerRichardson <- function(.ode) {
    euler <- new("Euler", .ode)
    euler <- init(euler, euler@stepSize)                         # diff 5
    return(euler)
}

