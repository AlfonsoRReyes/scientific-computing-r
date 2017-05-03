# Projectile.R
source("./R/ODE.R")
source("./R/Euler.R")

setClass("Projectile", slots = c(
    g = "numeric",
    odeSolver = "Euler"
    ),
    prototype = prototype(
        g = 9.8
    ),
    contains = c("ODE")
    )

setMethod("initialize", "Projectile", function(.Object) {
    # .Object@odeSolver <- new("Euler", .Object)
    # .Object@odeSolver <- Euler(.Object) <- gives error
    # .Object@odeSolver <- init(.Object@odeSolver, .Object)
    # .Object@odeSolver <- init(.Object@odeSolver, 0.1)   # no effect
    # cat("| 0 |")
    .Object@odeSolver <- Euler(.Object)                               # diff 6
    return(.Object)
})

setMethod("setStepSize", "Projectile", function(object, stepSize, ...) {
    object@odeSolver <- setStepSize(object@odeSolver, stepSize)
    object
})


setMethod("step", "Projectile", function(object) {
    # cat("Projectile:step:state:rate(1)", object@state, object@rate, "\n")
    object@odeSolver <- step(object@odeSolver)
    
    object@rate  <- object@odeSolver@ode@rate                           # diff 7
    object@state <- object@odeSolver@ode@state                          # diff 8
    
    # cat("Projectile:step:state:rate(2)", object@state, object@rate, "\n")
    object
})

setMethod("setState", "Projectile", function(object, x, vx, y, vy) {
    object@state[1] <- x
    object@state[2] <- vx
    object@state[3] <- y
    object@state[4] <- vy
    object@state[5] <- 0
    
    object@odeSolver@ode@state <- object@state
    object
})

setMethod("getState", "Projectile", function(object) {                 # diff 9
    object@state
})


setMethod("getRate", "Projectile", function(object, state, rate) {    # diff 10
    rate[1] <- state[2]                                               # diff 11
    rate[2] <- 0                                                      # diff 12
    rate[3] <- state[4]
    rate[4] <- - object@g
    rate[5] <- 1
    
    object@state <- object@odeSolver@ode@state <- state
    object@rate  <- object@odeSolver@ode@rate  <- rate
    object@rate                                                       # diff 18
})





# constructor
Projectile <- function()  new("Projectile")
