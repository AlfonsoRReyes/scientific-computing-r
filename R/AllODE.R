setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))
setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))
setGeneric("setState", function(object, x, vx, y, vy, ...) standardGeneric("setState"))
setGeneric("init", function(object, ...) standardGeneric("init"))

# setStepSize uses  two step parameters: stepSize and dt
# stepSize works for most of the applications
# dt is used in Pendulum
setGeneric("setStepSize", function(object, stepSize, dt, ...) 
    standardGeneric("setStepSize"),
    signature = c("object", "stepSize", "dt"))

# setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))



#########
# ODE.R
#

setClass("ODE", slots = c(
    state = "numeric",
    rate  = "numeric"
))


setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

setMethod("getRate", "ODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    return(object@rate)
})



# ODESolver.R
#

setClass("ODESolver", slots = c(
    stepSize = "numeric",
    numEqn   = "numeric",
    ode      = "ODE"
), prototype = prototype(
    stepSize = 0.1,
    numEqn = 0
)
)

setMethod("initialize", "ODESolver", function(.Object, .ode, ...) {
    .Object <- init(.Object, 0.1)                                 
    return(.Object)
})

setMethod("step", "ODESolver", function(object, ...) {
    # object
})

setMethod("setStepSize", "ODESolver", function(object, stepSize, ...) {
    object@stepSize = stepSize
    object
})

setMethod("init", "ODESolver", function(object, stepSize, ...) {
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        object@numEqn = length(state)
    }
    object
})

setMethod("getStepSize", "ODESolver", function(object, ...) {
    return(object@stepSize)
})


# constructor
ODESolver <- function(.ode) {
    odesolver <- new("ODESolver", .ode)
    odesolver@ode <- .ode
    odesolver
}



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
    # initialized the Euler ODE solver
    .Object@ode <- ode                          # set the ode to ODESolver slot
    .Object@ode@rate <- vector("numeric")       # create vector for the rate
    return(.Object)
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    object <- callNextMethod(object, stepSize)           # call superclass init
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object                                               #   right dimensions
})


setMethod("step", "Euler", function(object, ...) {
    # step through the differential equation
    state <- getState(object@ode)                         # get the state
    rate  <- getRate(object@ode, state, object@ode@rate)  # get the rate             
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]  # calc the new state
    }
    object@ode@state <- state              # return state and rate for new iter
    object@ode@rate  <- rate  
    object                                 # use this object to ressign in R
}) 

setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    # set the time step
    object@stepSize <-  stepSize
    object
})


setMethod("getStepSize", "Euler", function(object, ...) {
    return(object@stepSize)
})


# constructor ODE solver using Euler method
Euler <- function(.ode) {
    # Euler constructor
    euler <- new("Euler", .ode)                     # create the Euler object
    euler <- init(euler, euler@stepSize)            # iniialize Euler
    return(euler)
}



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
#     * @author             Wolfgang Christian
#     * converted to R by   Alfonso R. Reyes


setClass("EulerRichardson", slots = c(
    midstate = "numeric"                              # this is the midpoint slot
),
contains = c("ODESolver") 
)

setMethod("initialize", "EulerRichardson", function(.Object, ode, ...) {
    # initialize the class
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")        # create vector with no length
    return(.Object)
})


setMethod("init", "EulerRichardson", function(object, stepSize, ...) {
    # inititalize the solver
    object <- callNextMethod(object, stepSize)           # call superclass init
    object@ode@rate <- vector("numeric", object@numEqn)  # make the rate vector
    object@midstate <- vector("numeric", object@numEqn)  #    same size as state
    object
})


setMethod("step", "EulerRichardson", function(object, ...) {
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
EulerRichardson <- function(.ode) {
    # constructor for Euler-Richardson ODE solver
    eulerRichardson <- new("EulerRichardson", .ode)
    eulerRichardson <- init(eulerRichardson, eulerRichardson@stepSize)                         # diff 5
    return(eulerRichardson)
}