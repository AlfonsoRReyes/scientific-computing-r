# Pendulum.R

source("./R/ODE.R")
source("./R/Euler.R")


setGeneric("setState", function(object, x, vx, y, vy, ...) standardGeneric("setState"))

setClass("Pendulum", slots = c(
    omega0Squared = "numeric",
    state = "numeric",
    odeSolver = "Euler"
    ),
    prototype = prototype(
        omega0Squared = 3,
        state = c(0, 0, 0)
    ),
    contains = c("ODE")
    )

setMethod("initialize", "Pendulum", function(.Object) {
    .Object@odeSolver <- Euler(.Object)                               
    return(.Object)
})

setMethod("setStepSize", "Pendulum", function(object, stepSize, ...) {
    object@odeSolver <- setStepSize(object@odeSolver, stepSize)
    object
})


setMethod("step", "Pendulum", function(object) {
    object@odeSolver <- step(object@odeSolver)
    object@rate  <- object@odeSolver@ode@rate                           
    object@state <- object@odeSolver@ode@state                          
    object
})

setMethod("setState", "Pendulum", function(object, theta, thetaDot) {
    object@state[1] <- theta
    object@state[2] <- thetaDot
    object@odeSolver@ode@state <- object@state
    object
})

setMethod("getState", "Pendulum", function(object) {                
    object@state
})


setMethod("getRate", "Pendulum", function(object, state, rate) {    
    rate[1] <- state[2]     # rate of change of angle                                      # diff 11
    rate[2] <- -object@omega0Squared * sin(state[1])  # rate of change of dtheta                 
    rate[3] <- 1            # rate of change of time, dt/dt
    
    object@state <- object@odeSolver@ode@state <- state
    object@rate  <- object@odeSolver@ode@rate  <- rate
    object@rate                                                       # diff 18
})





# constructor
Pendulum <- function()  new("Pendulum")
