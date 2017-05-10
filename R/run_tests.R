library(testthat)

source("./R/ode_generics.R")
source("./R/ODE.R")
source("./R/ODESolver.R")
source("./R/AbstractODESolver.R")
source("./R/Euler.R")
source("./R/RK4.R")
source("./R/EulerRichardson.R")
source("./R/FallingParticleODE.R")

source("./R/Pendulum.R")
source("./R/Planet.R")
source("./R/Projectile.R")
source("./R/Reaction.R")

test_dir("./tests", reporter = "Summary")
