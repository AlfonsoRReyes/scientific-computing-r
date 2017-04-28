

setClass("FallingBall", slots = c(
    y = "numeric",
    v = "numeric",
    t = "numeric",
    dt = "numeric",
    G  = "numeric"   # G is the constant for gravity
))


setGeneric("step", function(object, ...) 
    standardGeneric("step"))
setGeneric("analyticalPosition", function(object, ...) 
    standardGeneric("analyticalPosition"))
setGeneric("analyticalVelocity", function(object, ...) 
    standardGeneric("analyticalVelocity"))
setGeneric("getY", function(object, ...) 
    standardGeneric("getY"))
setGeneric("getObservation", function(object, ...) 
    standardGeneric("getObservation"))


setMethod("initialize", "FallingBall",
          function(.Object, y, v, t, dt){
              .Object@y  <- y
              .Object@v  <- v
              .Object@t  <- t
              .Object@dt <- dt
              .Object@G  <- 9.8      # this is aconstant in the class
              return(.Object)
          })

setMethod("step", "FallingBall", function(object, ...) {
    object@y <- object@y + object@v * object@dt
    object@v <- object@v - object@G * object@dt
    object@t <- object@t + object@dt
    return(object)
})

setMethod("analyticalPosition", "FallingBall", function(object, y0, v0, ...) {
    return(y0 + v0 * object@t - 0.5 * object@G * object@t^2)
})

setMethod("analyticalVelocity", "FallingBall", function(object, v0, ...) {
    return(v0 * 9.8 * object@t)
})

# a getter to extract the Y position
setMethod("getY", "FallingBall", function(object) {
    return(object@y)
})

# get a row of data
setMethod("getObservation", "FallingBall", function(object) {
    obs <- list(velocity = object@v, ycoord = object@y, t = object@t, dt = object@dt)
    return(obs)
})



# constructor
FallingBall <- function(y, v, t, dt) {
    new("FallingBall", y = y, v = v, t = t, dt = dt)
}

