# Projectile.R
source("./R/ODE.R")
source("./R/Euler.R")

setClass("Projectile", slots = c(
    g = "numeric",
    # state = "numeric",
    # test_1 = "numeric",
    odeSolver = "Euler"
    ),
    prototype = prototype(
        g = 9.8
    ),
    contains = c("ODE")
)

setMethod("initialize", "Projectile", function(.Object) {
    # .Object@odeSolver <- Euler(.Object)
    # ode <- new("ODE")
    # .Object@odeSolver <- new("Euler", ode)
    .Object@odeSolver <- new("Euler", .Object)
    # .Object@rate <- ode@rate
    # .Object@state <- ode@state
    # .Object <- .Object@odeSolver@ode
    # cat(".Object@odeSolver@ode@rate=", .Object@odeSolver@ode@rate, "\n")
    # cat(".Object@rate==", .Object@rate, "\n")
    # cat(".Object@state=", .Object@state, "\n")
    # cat("Projectile:initialize:.Object@odeSolver=", class(.Object@odeSolver), "\n")
    # return(.Object)
    # ret <- callNextMethod()
    # cat(class(ret))
    return(.Object)
    
})

setMethod("setStepSize", "Projectile", function(object, stepSize, ...) {
    object@odeSolver <- setStepSize(object@odeSolver, stepSize)
    object
})


setMethod("step", "Projectile", function(object) {
    # cat("Projectile:step:state:rate(1)", object@state, object@rate, "\n")
    object@odeSolver <- step(object@odeSolver)
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

# constructor

Projectile <- function()  new("Projectile")

