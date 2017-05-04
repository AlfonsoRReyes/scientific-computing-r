

setClass("Player", 
         slots = c(
             prob = "numeric",
             score = "numeric",
             ID = "character"), 
         prototype = list(
             prob = 0.5,
             score = 0,
             ID = "X"
         ))


setClass("RBallGame", slots = c(
    PlayerA = "Player", 
    PlayerB = "Player", 
    server  = "Player")
)

setClass("SimStats", slots = c(
    winsA = "numeric",
    winsB = "numeric",
    shutsA = "numeric",
    shutsB = "numeric"
))

#' function to compare if two S4 objects are the same
id <- function(a,b){
    sapply(slotNames(a),function(x)identical(slot(a,x),slot(b,x)))
}


# generic for Player methods
setGeneric("winsServe", function(object, ...) standardGeneric("winsServe"))
setGeneric("incScore", function(object, ...) standardGeneric("incScore"))
setGeneric("getScore", function(object, ...) standardGeneric("getScore"))
setGeneric("whois", function(object, ...) standardGeneric("whois"))

# generics for RBallGame
setGeneric("play", function(object, ...) standardGeneric("play"))
setGeneric("isOver", function(object, ...) standardGeneric("isOver"))
setGeneric("changeServer", function(object, ...) standardGeneric("changeServer"))
setGeneric("getScores", function(object, ...) standardGeneric("getScores"))

# generic function for SimStats
setGeneric("Update", function(object, aGame) standardGeneric("Update"))
setGeneric("printReport", function(object, ...) standardGeneric("printReport"))
setGeneric("printLine", function(object, ...) standardGeneric("printLine"))


# methods for Player
setMethod("show",
          signature = "Player",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Prob: ", object@prob, " ID: ", object@ID,
                  " Score: ", object@score, sep = "")
              #invisible(NULL)
          })


setMethod("initialize", "Player",
          function(.Object, prob, ID){
              # callNextMethod(.Object, ...)
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
    return(object@score)
})

setMethod("getScore", "Player", function(object) {
    return(object@score)})

setMethod("whois", "Player", function(object) {
    return(object@ID)
})

# Player <- function(prob) new("Player", prob = prob)
# p <- Player(0.5)



#############################################################################
# set methods for RBAllGame
setMethod("initialize", "RBallGame",
          function(.Object, ..., probA = 0.5, probB = 0.5){
              .Object@PlayerA <- Player(prob = probA, ID = "A")
              .Object@PlayerB <- Player(prob = probB, ID = "B")
              .Object@server <- .Object@PlayerA
              #callNextMethod(.Object, ...)
              return(.Object)
          })


setMethod("play", "RBallGame", function(object) {
    cat(whois(object@server), "\n")
    #object <- changeServer(object)
    # object@server <- 
    object <- changeServer(object)
    # object@server <- incScore(object@server)
    cat(whois(object@server), "\n")
    slot(object@server, "score") <- incScore(object@server)
    cat("\t", getScore(object@server))
    # cat(whois(object@server), "\n")
    # i = 1
    # while (!isOver(object)) {
    #     cat(i, "\t", whois(object@server), "\t")
    #     i <-  i + 1
    #     if (i > 20) break
    #     if (winsServe(object@server)) {
    #         cat("inc")
    #         # object@server <- incScore(object@server)
    #         incScore(object@server)
    #         cat("\t", getScore(object@server))
    #     } else {
    #         cat("chg")
    #         changeServer(object)
    #     }
    # }
    
    return(object)
})

setMethod("isOver", "RBallGame", function(object) {
    os <- getScores(object)
    a <- os[["A"]]
    b <- os[["B"]]
    # cat(a, b)
    res <- a == 15 || b == 15 || (a == 7 && b == 0) || (b == 7 && a == 0)
    return(res)
})

setMethod("changeServer", "RBallGame", function(object) {
    # if (all(id(object@server, object@PlayerA))) {
    if (slot(object@server, "ID") == "A") {
        object@server <-  object@PlayerB
        cat(getScore(object@PlayerB), "playerB\n")
        cat("cS-if-A ->", whois(object@server), "\n")
    }  else {
        object@server <- object@PlayerA
        cat(getScore(object@PlayerA), "playerA\n")
    }
    return(object)
})

setMethod("getScores", "RBallGame", function(object) {
    A <- getScore(object@PlayerA)
    B <- getScore(object@PlayerB)
    return(list(A = A, B = B))
    # return(object)
})

# RBallGame <- function(probA, probB) new("RBallGame", probA = probA, probB = probB)

########################################################################################



# set method for SimStats
setMethod("initialize", "SimStats",
          function(.Object, ...) {
              .Object@winsA <- 0
              .Object@winsB <- 0
              .Object.shutsA <-  0
              .Object.shutsB <-  0
              #callNextMethod(.Object, ...)
              return(.Object)
          })

setMethod("Update", "SimStats", function(object, aGame) {
    a <- getScores(aGame)[["A"]]
    b <- getScores(aGame)[["B"]]
    cat("scores", a, b, "\n")
    if (a > b) {
        object@winsA <- object@winsA + 1
        if (b == 0)
            object@shutsA <- object@shutsA + 1
    } else {
        object@winsB <- object@winsB + 1
        if (a == 0)
            object@shutsB <- object@shutsB + 1
    }
    return(object)
})

# constructors
Player <- function(prob, ID) new("Player", prob = prob, ID = ID)
RBallGame <- function(probA, probB) new("RBallGame", probA = probA, probB = probB)
SimStats <- function() new("SimStats")
