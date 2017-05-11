source("./R/DormandPrince45.R")


setClass("RK45", 
    contains = c("DormandPrince45") 
)

setMethod("initialize", "RK45", function(.Object, ode, ...) {
    # initialized the ODE solver
    .Object@ode <- ode                          # set the ode to ODESolver slot
    return(.Object)
})



# constructor ODE solver using Euler method
RK45 <- function(.ode) {
    # Euler constructor
    rk45 <- new("RK45", .ode)                     # create the Euler object
    return(rk45)
}

