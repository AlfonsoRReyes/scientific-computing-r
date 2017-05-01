setGeneric("getRate", function(object, state, rate, ...) standardGeneric("getRate"))
setGeneric("getState", function(object, ...) standardGeneric("getState"))
setGeneric("init", function(object, stepSize, ...) standardGeneric("init"))
setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("setStepSize", function(object, stepSize, ...) standardGeneric("setStepSize"))
setGeneric("getStepSize", function(object, ...) standardGeneric("getStepSize"))

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
    return(.Object)
})

setMethod("getState", "FallingParticleODE", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

setMethod("getRate", "FallingParticleODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    # cat("getRate()  called with ", class(object), "\n")
    object@rate[1] <- state[2]
    object@rate[2] <- - object@g
    object@rate[3] <- 1
    object@rate   # last change
    
})

# constructor
FallingParticleODE <- function(y, v) {
    .FallingParticleODE <- new("FallingParticleODE")
    .FallingParticleODE@state[1] <- y
    .FallingParticleODE@state[2] <- v
    .FallingParticleODE@state[3] <- 0
    .FallingParticleODE
}

# ----------------------------------------- Euler --------------------
setClass("Euler", slots = c(
    stepSize = "numeric",
    numEqn   = "numeric",
    ode      = "ODE"
), prototype = prototype(
    stepSize = 0.1,
    numEqn = 0
)
)

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    # .Object <- callNextMethod(.Object, ode)
    .Object@ode <- ode
    .Object@ode@rate <- vector("numeric")
    return(.Object)
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    # cat("Euler:init \n")
    # object <- initSolver(object, stepSize)
    object@stepSize <- stepSize
    state <- getState(object@ode)
    object@ode@state <- state
    
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        # cat("assigning a value > 0 \n")
        object@numEqn = length(state)
        # cat("ODESOlver:object@numEqn =", object@numEqn, "\n")
    }
    object@ode@rate <- vector("numeric", object@numEqn)
    object
})


setMethod("step", "Euler", function(object, ...) {
    # who calls `step()`? : it is called from the main application, from a loop
    state <- getState(object@ode)
    rate  <- getRate(object@ode, state, object@rate)
    
    # cat("Euler:step:state:", state, "\n")
    # cat("Euler:step:rate:", object@ode@rate, "\n")
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
    }
    # cat("Euler:step:state:", state, "\n")
    object@ode@state <- state
    object@ode@rate  <- rate  # does rate has to return?
    object
}) 

setMethod("setStepSize", "Euler", function(object, stepSize, ...) {
    object@stepSize = stepSize
    # cat("step size is ", object@stepSize, "\n")
    object
})


setMethod("getStepSize", "Euler", function(object, ...) {
    object <- object
    return(object@stepSize)
})



# constructor
Euler <- function(.ode) {
    euler <- new("Euler", .ode)
    # cat("euler@stepSize:", euler@stepSize, "\n")
    euler <- init(euler, euler@stepSize)
    return(euler)
}



