# Racquetball classes
# from Python to R S4 classes

setClass("Player",
    slots = c(prob = "numeric",
              score = "numeric",
              ID = "character"),
    prototype = list(prob = 0.5,
                     score = 0,
                     ID = "X"
    ))


setClass("RBallGame",
         slots = c(
             PlayerA = "Player",
             PlayerB = "Player",
             server  = "Player"
    ))

setClass("SimStats",
    slots = c(
        winsA  = "numeric",
        winsB  = "numeric",
        shutsA = "numeric",
        shutsB = "numeric",
        games  = "numeric"
    )
)

#' function to compare if two S4 objects are the same
id <- function(a,b){
    sapply(slotNames(a),function(x)identical(slot(a,x),slot(b,x)))
}


# generic functions for Player methods
setGeneric("winsServe", function(object, ...) standardGeneric("winsServe"))
setGeneric("incScore", function(object, ...) standardGeneric("incScore"))
setGeneric("getScore", function(object, ...) standardGeneric("getScore"))
setGeneric("whois", function(object, ...) standardGeneric("whois"))

# generics functions for RBallGame
setGeneric("play", function(object, ...) standardGeneric("play"))
setGeneric("isOver", function(object, ...) standardGeneric("isOver"))
setGeneric("changeServer", function(object, ...) standardGeneric("changeServer"))
setGeneric("getScores", function(object, ...) standardGeneric("getScores"))

# generic functions for SimStats
setGeneric("Update", function(object, aGame) standardGeneric("Update"))
setGeneric("printReport", function(object, ...) standardGeneric("printReport"))
setGeneric("printLine", function(object, ...) standardGeneric("printLine"))

###############################################################################
# methods for Player class
setMethod("show",
          signature = "Player",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Prob: ", object@prob, " ID: ", object@ID,
                  " Score: ", object@score, sep = "")
          })


setMethod("initialize", "Player",
          function(.Object, prob, ID){
              .Object@score <- 0
              .Object@prob  <- prob
              .Object@ID    <-  ID
              return(.Object)
          })

setMethod("winsServe", "Player", function(object) {
    ru <- runif(1)
    return(runif(1) <= object@prob)})

setMethod("incScore", "Player", function(object) {
    object@score <- object@score + 1
    # return(object@score)
    object
})

setMethod("getScore", "Player", function(object) {
    return(object@score)})

setMethod("whois", "Player", function(object) {
    return(object@ID)
})


###############################################################################
# set methods for RBAllGame class
setMethod("initialize", "RBallGame",
          function(.Object, ..., probA = 0.5, probB = 0.5){
              .Object@PlayerA <- Player(prob = probA, ID = "A")
              .Object@PlayerB <- Player(prob = probB, ID = "B")
              .Object@server <- .Object@PlayerA
              return(.Object)
          })


setMethod("play", "RBallGame", function(object) {
    # This implementation is different in Python. In R, values between methods
    # are passed by value not reference as in Python.
    if (RBall.env[["verbose"]]) cat(sprintf("%35s %7s %7s %8s %8s \n", 
                                            "#", "Player", "Done", "Score A", 
                                            "Score B"))
    i <- 1
    while (!isOver(object)) {
        if (winsServe(object@server)) {
            if (whois(object@server) == "A") 
                object@PlayerA <- incScore(object@PlayerA) # explicit assignment
            else
                object@PlayerB <- incScore(object@PlayerB) # explicit assignment
        } else {
            object <- changeServer(object)                 # explicit assignment
        }
        if (RBall.env[["verbose"]]) cat(sprintf("%35d %7s %7s %8d %8d \n", 
                            i, whois(object@server), isOver(object), 
                            getScore(object@PlayerA), getScore(object@PlayerB)))
        i <- i +1
    }
    object
})

setMethod("isOver", "RBallGame", function(object) {
    score <- getScores(object)
    a <- score[["A"]]
    b <- score[["B"]]
    done <- a == 15 || b == 15 || (a == 7 && b == 0) || (b == 7 && a == 0)
    return(done)
})

setMethod("changeServer", "RBallGame", function(object) {
    if (slot(object@server, "ID") == "A") {
        object@server <-  object@PlayerB
    } else {
        object@server <- object@PlayerA
    }
    object
})

setMethod("getScores", "RBallGame", function(object) {
    return(list(A = getScore(object@PlayerA), 
                B = getScore(object@PlayerB)))
})

###############################################################################
# set method for SimStats class
setMethod("initialize", "SimStats",
          function(.Object, ...) {
              .Object@winsA  <- 0
              .Object@winsB  <- 0
              .Object@shutsA <- 0
              .Object@shutsB <- 0
              .Object@games  <- 0
              # header for report
              cat("-------- Scores -------- \n")
              cat(sprintf("%7s %7s %7s \n", "Game", "A", "B"))
              return(.Object)
          })

setMethod("Update", "SimStats", function(object, aGame) {
    object@games <- object@games + 1
    a <- getScores(aGame)[["A"]]
    b <- getScores(aGame)[["B"]]
    cat(sprintf("%7d %7d %7d \n", object@games, a, b))
    if (a > b) {
        object@winsA <- object@winsA + 1
        if (b == 0)
            object@shutsA <- object@shutsA + 1
    } else {
        object@winsB <- object@winsB + 1
        if (a == 0)
            object@shutsB <- object@shutsB + 1
    }
    object
})

setMethod("printReport", "SimStats", function(object) {
    n <- object@winsA + object@winsB
    cat("\nNumber of games:", n, "\n")
    cat(sprintf("%7s %5s %5s \n", "Player", "Wins", "Shuts"))
    cat(sprintf("%7s %5d %5d \n", "A", object@winsA, object@shutsA))
    cat(sprintf("%7s %5d %5d \n", "B", object@winsB, object@shutsB))
})

setMethod("printLine", "SimStats", function(object, aGame) {
})

# create a project environment
RBall.env <- new.env(parent = emptyenv()) # create new environment

# constructors
Player <- function(prob, ID) new("Player", prob = prob, ID = ID)
RBallGame <- function(probA, probB, verbose = FALSE) {
    # use an environment to pass the verbose flag for printing
    RBall.env[["verbose"]] <- verbose
    new("RBallGame", probA = probA, probB = probB)
}    
SimStats <- function() new("SimStats")
