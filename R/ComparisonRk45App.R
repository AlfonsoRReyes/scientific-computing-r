source("./R/ode_generics.R")
source("./R/RK45.R")
source("./R/ODETest.R")

ode <- new("ODETest")

ode_solver <- RK45(ode)

ode_solver <- setStepSize(ode_solver, 1)
ode_solver <- setTolerance(ode_solver, 1e-8)

time <-  0
state <- getState(ode)
state
ode_solver@ode@state <- state
ode_solver@numStages
ode_solver@numEqn
ode_solver@tol
ode_solver@stepSize

while (time < 50) {
    time <- time + 1
    ode_solver <- step(ode_solver)
    state <- getState(ode_solver@ode)
    xStrl <- "xl" + state[1]
    cat("time=", time, xStrl, "\n")
}