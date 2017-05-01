source("./R/ode_generics.R")


setClass("ODE", slots = c(
    state = "numeric",
    rate  = "numeric"
))


setMethod("initialize", "ODE", function(.Object, ...) {
    # cat("initializing class: ", class(.Object), "\n")
    .Object@state <- vector("numeric")
    .Object@rate  <- vector("numeric")
    return(.Object)
    # callNextMethod(.Object)
})

setMethod("getState", "ODE", function(object, ...) {
    # Gets the state variables.
    # cat("getState() called with ", class(object), "\n")
    # return(object@state)
})

setMethod("getRate", "ODE", function(object, state, rate, ...) {
    # Gets the rate of change using the argument's state variables.
    # cat("getRate()  called with ", class(object), "\n")
    # return(object@rate)
})