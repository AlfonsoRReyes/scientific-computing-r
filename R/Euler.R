source("./R/ode_generics.R")
source("./R/ODESolver.R")


setClass("Euler", slots = c(
    rate = "numeric"
     ), 
contains = c("ODESolver")
)

setMethod("initialize", "Euler", function(.Object, ode, ...) {
    # .Object <- callNextMethod(.Object, ode)
    # .Object@ode <- ode
    .Object@rate <- vector("numeric")
    # return(.Object)
    callNextMethod(.Object, .Object@ode)    # this calls the parent class
})


setMethod("init", "Euler", function(object, stepSize, ...) {
    # cat("Euler:init \n")
    object <- initSolver(object, stepSize)
    # object@rate <- object@numEqn
    cat("Euler:object@numEqn =", object@numEqn, "\n")
    object@rate <- vector("numeric", object@numEqn)
    cat("Euler:init:rate =", object@rate, "\n")
    object
})


setMethod("step", "Euler", function(object, ...) {
    # who calls `step()`? : it is called from the main application, from a loop
    state <- getState(object@ode)
    # cat("Euler:step:rate:", object@rate, "\n")
    # cat("Euler:step:state:", state, "\n")
    # cat("object@stepSize:", object@stepSize, "\n")
    # rate  <- getRate(object@ode, object@ode@state, object@rate)
    
    rate  <- getRate(object@ode, state, object@rate)
    cat("after:rate:", object@rate, "\n")
    
    for (i in 1:object@numEqn) {
        state[i] <- state[i] + object@stepSize * rate[i]
        cat(i, state[i], rate[i], "\n")
    }
    # # return(object@stepSize)
    # object@ode@state <- state
    # object@rate  <- rate  # does rate has to return?
    object
}) 


# constructor
Euler <- function(.ode) {
    euler <- new("Euler", .ode)
    # cat("euler@stepSize:", euler@stepSize, "\n")
    euler <- init(euler, euler@stepSize)
    return(euler)
}


# setMethod("step", "Euler", function(object, ...) {
#     # who calls `step()`? : it is called from the main application, from a loop
#     cat(sprintf("%25s  %11s  %11s \n", "state_1", 
#                 "state_2", "state_3"))
#     
#     object@ode@state <- getState(object@ode)
#     object@ode@rate  <- getRate(object@ode, object@ode@state, object@rate)
#     
#     cat(sprintf("%12s %12f %12f %12f \n", "Euler:state", 
#                 object@ode@state[1], object@ode@state[2], object@ode@state[3]))
#     cat(sprintf("%12s %12f %12f %12f \n", "Euler:rate", 
#                 object@ode@rate[1], object@ode@rate[2], object@ode@rate[3]))
#     
#     # cat("Euler:rate   =", rate, "\n")
#     # cat("Euler:numEqn =", object@numEqn, "\n")
# 
#     cat(sprintf("%12s", "state(iter)"))
#     
#     for (i in 1:object@numEqn) {
#         object@ode@state[i] <- object@ode@state[i] + object@stepSize * object@rate[i]
#         
#         # cat(sprintf("%13f", object@ode@state[i]))
#         cat(sprintf("%13d", i))
#     }
#     cat("\n")
#     # return(object@stepSize)
#     # object@ode@state <- state
#     # object@ode@rate  <- rate
#     object
# })    