setGeneric("init", function(object, .stepSize, ...) standardGeneric("init"))
setGeneric("init", function(object, .initState, ...) standardGeneric("init"))
setGeneric("step", function(object, ...) standardGeneric("step"))
setGeneric("getState", function(object, time, ...) standardGeneric("getState"))
setGeneric("getRate", function(object, .state, .rate, ...) standardGeneric("getRate"))
setGeneric("setStepSize", function(object, .stepSize, ...) standardGeneric("setStepSize"))
setGeneric("getStepSize", function(object, time, ...) standardGeneric("getStepSize"))
setGeneric("doStep", function(object, ...) standardGeneric("doStep"))


setClass("ODE", slots = c(
    state = "numeric",
    rate  = "numeric",
    whois = "character"
))


setMethod("initialize", "ODE", function(.Object, ...) {
    cat("initializing class: ", class(.Object), "\n")
    .Object@whois <- "ODE"
    return(.Object)
})
setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    cat("getState() called with ", class(object), "\n")
})

setMethod("init", "ODE", function(object, .initState, ...) {
    cat("init at ", class(object), "\n")
    object
})

setMethod("getRate", "ODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    cat("getRate()  called with ", class(object), "\n")
})

##############################################################################
setClass("ODESolver")

setMethod("initialize", "ODESolver", function(.Object, .ode, ...) {
    .Object@whoIam <- "ODESolver"
    return(.Object)
})

setMethod("init", "ODESolver", function(object, .stepSize, ...) {
    # this method is not in the original Java
    cat("ODESolver \n")
    object
})

setMethod("step", "ODESolver", function(object, ...) {
    cat("step() \n")
    object
})

setMethod("setStepSize", "ODESolver", function(object, .stepSize, ...) {
})

setMethod("getStepSize", "ODESolver", function(object, ...) {
})


##############################################################################
setClass("AbstractODESolver", slots = c(
    stepSize = "numeric",
    numEqn   = "numeric",
    ode      = "ODE"
    ), 
    contains = c("ODESolver")
)

setMethod("initialize", "AbstractODESolver", function(.Object, .ode, ...) {
    .Object@ode <- .ode
    init(.Object@ode, 0.1)
    cat("initializing class: ", class(.Object), "\n")
    return(.Object)
})

setMethod("step", "AbstractODESolver", function(object, ...) {
    cat("step() at class ", class(object), "\n")
    object@stepSize = 5
    # step(object)
})



setMethod("init", "AbstractODESolver", function(object, .stepSize, ...) {
    object@stepSize <- .stepSize
    # state <- object@ode@getState()
    state <- getState(object@ode)
    cat("length(state): ", length(state), "\n")
    
    if (is.null(state)) {
        object@numEqn <-  0
    } else {
        
        object@numEqn = length(state)
    }
    cat("ODESolver \n")
})

setMethod("setStepSize", "AbstractODESolver", function(object, .stepSize, ...) {
    object@stepSize = .stepSize
    cat("step size is ", object@stepSize, "\n")
    object
})

setMethod("getStepSize", "AbstractODESolver", function(object, ...) {
    return(object@stepSize)
})

##############################################################################
setClass("Euler", slots = c(
    ode  = "ODE",
    rate = "numeric"
    ), contains = c("AbstractODESolver")
)

setMethod("initialize", "Euler", function(.Object, .ode, ...) {
    cat("initializing class: ", class(.Object), "\n")
    ode <- .ode
    init(ode)
    .Object@rate <- 0
    return(.Object)
})

setMethod("init", "Euler", function(object, .stepSize, ...) {
    init(object, .stepSize)
    object@rate <- slot("AbstractODESolver", "numEqn")
})    

setMethod("step", "Euler", function(object, ...) {
    state <- getState(object@ode)
    cat("class(state): ", class(state), "\n")
    getRate(ode, state, object@rate)
    object@numEqn <- 3
    cat(object@numEqn, "\n")
    for (i in 0:object@numEqn) {
        state[i] <- state[i] + object@stepSize * object@rate[i]
    }
    return(object@stepSize)
})    

# constructor

Euler <- function(.ode) {
    new("Euler", .ode)
}


# 
# setClass("Planet", slots = c(
#     state     = "numeric",
#     solver = "Euler"
# ))
