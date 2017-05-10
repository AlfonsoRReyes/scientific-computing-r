source("./R/ODEAdaptiveSolver.R")
source("./R/ODE.R")

setClass("DormandPrince45", slots = c(
    error_code = "numeric",
    a = "matrix",
    b5 = "numeric",
    er = "numeric",
    numStages = "numeric",
    stepSize = "numeric",
    numEqn = "numeric",
    temp_state = "numeric",
    k = "matrix",
    truncErr = "numeric",
    ode = "ODE",
    tol = "numeric",
    enableExceptions = "logical"
    ), 
    contains = c("ODEAdaptiveSolver")
    )

setMethod("initialize", "DormandPrince45", function(.Object, ode, ...) {
    # initialized the Euler ODE solver
    .Object@a <- rbind( c(1.0/5.0, 0, 0, 0, 0), 
                c(3.0/40.0, 9.0/40.0, 0, 0, 0), 
                c(3.0/10.0, -9.0/10.0, 6.0/5.0, 0, 0), 
                c(226.0/729.0, -25.0/27.0, 880.0/729.0, 55.0/729.0, 0), 
                c(-181.0/270.0, 5.0/2.0, -266.0/297.0, -91.0/27.0, 189.0/55.0))
    
    .Object@b5 <- c(19.0/216.0, 0.0, 1000.0/2079.0, -125.0/216.0, 81.0/88.0, 5.0/56.0)
    .Object@er <- c(-11.0/360.0, 0.0, 10.0/63.0, -55.0/72.0, 27.0/40.0, -11.0/280.0)
    .Object@numStages <- 6
    .Object@stepSize <- 0.01
    .Object@numEqn <- 0
    .Object@tol <- 1E-6
    .Object@enableExceptions <- FALSE
    .Object@ode <- ode                          # set the ode to ODESolver slot
    
    .Object@ode@rate <- vector("numeric")       # create vector for the rate
    return(.Object)
})


setMethod("init", "DormandPrince45", function(object, stepSize, ...) {
    # inititalize the solver
    object@stepSize <- stepSize
    state <- getState(object@ode)
    if (is.null(state)) return()
    if (object@numEqn != length(state)) {
        object@numEqn <- length(state)
        object@temp_state <- vector("numeric", numEqn)
        object@k <- matrix(nrow= object@numStages,ncol =  object@numEqn)
    }
    object
})


setMethod("step", "DormandPrince45", function(object) {
    object@error_code <- object@NO_ERROR
    iterations <- 10
    currentStep <- object@stepSize <- error <- 0
    state <- getState(object@ode)
    rate <- getRate(object@ode, state, object@k[0])
    repeat  {
        currentStep <- object@stepSize
        # compute the k's
        for (s in 2:object@numStages) {
            for (i in 1:numEqn) {
                object@temp_state[i] <- state[i]
                for (j in 1:s) {
                    object@temp_state[i] <- object@temp_state[i] + object@stepSize *
                        object@a[s-1][j] * k[j][i]
                }
            }
            rate <- getRate(object@ode, object@temp_state, k[s])
        }
        # compute the error
        error <-  0
        for (i in 1:numEqn) {
            object@truncErr <- 0
            for (s in 1:object@numStages) {
                object@truncErr <- object@truncErr + object@stepSize * object@er[s] *
                    k[s][i]
            }
            error <- max(error, abs(object@truncErr))
        }
        if (error <= 1.4E-45) {   # error to small
            error <- tol / 1.0e5 # increase step size x10
        }
        # find h step for the next try
        if (error > tol) {
            fac <- 0.9 * (error/tol)^-0.25
            object@stepSize <- object@stepSize * max(fac, 0.1)
        } else if (error < tol / 10.0) {
            fac <- 0.9 * (error/tol)^-0.2
            if (fac > 1)
                object@stepSize <- object@stepSize * max(fac, 10)
        }
        
        if (error > object@tol && iterations > 0) break
    }   # end repeat loop   
    # advance the state
    for (i in 1:object@numEqn) {
        for (s in 1:object@numStages) {
            state[i] <- state[i] + currentStep * object@b5[s] * 
                object@k[s][i]
        }
    }
    if (iterations == 0) {
        object@error_code <- object@DID_NOT_CONVERGE
        if (object@enableExceptions) {
            warning("DormandPrince45 ODE SOlver did not converge")
        }
    }
    return(currentStep)
}
)


DormandPrince45 <- function(ode) {
    dormandPrince45 <- new("DormandPrince45", ode)
    dormandPrince45 <- init(dormandPrince45, dormandPrince45@stepSize)
}