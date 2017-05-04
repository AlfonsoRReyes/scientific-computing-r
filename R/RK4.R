source("./R/ODESolver.R")



setClass("RK4", slots = c(
  rate1 = "numeric",
  rate2 = "numeric",                             
  rate3 = "numeric", 
  rate4 = "numeric",       
  estimated_rate = "numeric"                           
),
    contains = c("ODESolver") 
)

setMethod("initialize", "RK4", function(.Object, ode, ...) {
    # initialize the class
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")        # create vector with no length
    return(.Object)
})


setMethod("init", "RK4", function(object, stepSize, ...) {
    # inititalize the solver
    object <- callNextMethod(object, stepSize)           # call superclass init
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object@midstate <- vector("numeric", object@numEqn)  #    same size as state
    object
})


setMethod("step", "RK4", function(object, ...) {
    # step through the diffrential equation
    state <- getState(object@ode)                         # get the state vector
    rate  <- getRate(object@ode, state, object@ode@rate)  # get the rate vector
    
    dt2 <- object@stepSize / 2                            # divide stepSize
    
    for (i in 1:object@numEqn) {
        # estimate the state at the midpoint
        object@midstate[i] <- state[i] + rate[i] * dt2
    }
    
    rate  <- getRate(object@ode, object@midstate, rate) # rate based on midpoint
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i] # calc new state
    }
    
    object@ode@state <- state       # return state and rate for new iter
    object@ode@rate  <- rate  
    object                          # use this object to reassign in R
}) 


# constructor
RK4 <- function(.ode) {
    # constructor for Euler-Richardson ODE solver
    rk4 <- new("RK4", .ode)
    rk4 <- init(eulerRichardson, rk4@stepSize)                        
    return(rk4)
}

