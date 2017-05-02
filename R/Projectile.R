# Projectile.R

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
    .Object@odeSolver <- Euler(.Object)
    .Object <- .Object@odeSolver@ode
    cat(".Object@odeSolver@ode@rate=", .Object@odeSolver@ode@rate, "\n")
    cat(".Object@rate=", .Object@rate, "\n")
    cat(".Object@state=", .Object@state, "\n")
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
    object@odeSolver <- step(object@odeSolver)
    object
})


# constructor

Projectile <- function()  new("Projectile")