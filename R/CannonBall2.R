setGeneric("update", function(object) standardGeneric("update"))
setGeneric("getY", function(object) standardGeneric("getY"))
setGeneric("getX", function(object) standardGeneric("getX"))
setGeneric("setInterval", function(object, time) standardGeneric("setInterval"))



setClass("Projectile", slots = c(
    xpos = "numeric",
    ypos = "numeric",
    xvel = "numeric",
    yvel = "numeric"
))

setClass("CannonBall", slots = c(
    time = "numeric",
    g    = "numeric"
), prototype = prototype(
    g = 9.8
),
         contains = c("Projectile"))

setMethod("getY", "Projectile", function(object) {
    return(object@ypos)
})

setMethod("getX", "Projectile", function(object) {
    return(object@xpos)
})




setMethod("initialize", "CannonBall",
      function(.Object, xpos = xpos, ypos = ypos, xvel = xvel, yvel = yvel){
          .Object@xpos <- xpos
          .Object@ypos <- ypos
          .Object@xvel <- xvel
          .Object@yvel <- yvel
          return(.Object)
      })

setMethod("update", "CannonBall", function(object) {
    object@xpos <-  object@xpos + object@time * object@xvel
    yvel1 <- object@yvel - object@g * object@time
    object@ypos <-  object@ypos + object@time * (object@yvel +  yvel1) / 2.0
    object@yvel <-  yvel1
    return(object)
})

setMethod("getY", "CannonBall", function(object) {
    return(object@ypos)
})

setMethod("getX", "CannonBall", function(object) {
    return(object@xpos)
})

setMethod("setInterval", "CannonBall", function(object, time) {
    object@time <- time
    object
})

# constructor
CannonBall <- function(angle, velocity, height) {
    xpos <-  0
    ypos <-  height
    theta <- pi * angle / 180.0
    xvel <- velocity * cos(theta)
    yvel <- velocity * sin(theta)
    new("CannonBall", xpos, ypos, xvel, yvel)
}

