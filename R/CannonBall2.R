setGeneric("update", function(object, time) standardGeneric("update"))
setGeneric("getY", function(object) standardGeneric("getY"))
setGeneric("getX", function(object) standardGeneric("getX"))


setClass("Projectile", slots = c(
    xpos = "numeric",
    ypos = "numeric",
    xvel = "numeric",
    yvel = "numeric"
))


setClass("CannonBall", contains = c("Projectile"))

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

setMethod("update", "CannonBall", function(object, time) {
    object@xpos <-  object@xpos + time * object@xvel
    yvel1 <- object@yvel - 9.8 * time
    object@ypos <-  object@ypos + time * (object@yvel +  yvel1) / 2.0
    object@yvel <-  yvel1
    return(object)
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