# ************************************************************* Test ODESolver
library(testthat)

source("./R/ode_generics.R")
source("./R/ODE.R")
source("./R/ODESolver.R")

context("test ODESolver")

test_that("there are no constructor for ODE", {
    expect_error(ode <- ODE(), 'could not find function "ODE"')
})

test_that("Constructor needs ODE parameter", {
    expect_error(ODESolver(), 'argument ".ode" is missing, with no default')
})


ode <- new("ODE")
odesolver <- ODESolver(ode)

test_that("Class is correct", {
    expect_true(class(odesolver) == "ODESolver")
})

test_that("Have correct slot names", {
    expect_equal(slotNames(odesolver), c("stepSize", "numEqn", "ode"))
    expect_equal(slotNames(odesolver@ode), c("state", "rate"))
})


state <- c(0, 1, 3)

odesolver@ode@state <-  c(0, 1, 3)             # set a vector for state

test_that("Get the default step size", {
    expect_equal(getStepSize(odesolver), 0.1)        # get default step size
})


# ----------------------------------------- these two vars hold no values
expect_true(length(ode@state) == 0)
expect_true(length(ode@rate)  == 0)

# +++++++++++++++++++++++++++++++++++++++++ values stored here
expect_true(all(odesolver@ode@state == state))
expect_true(length(odesolver@ode@rate) == 0)

odesolver <- init(odesolver, 0.123)
test_that("values set after init", {
    expect_equal(odesolver@ode@state, state)
    expect_true(length(odesolver@ode@rate) == 0)
    expect_equal(odesolver@numEqn, 3)
})


test_that("Can set and get step size", {
    odesolver <- setStepSize(odesolver, 0.1010)     # set a new step size
    expect_equal(getStepSize(odesolver), 0.1010)
})


expect_true(is.null(step(odesolver)))           # step from odesolver NULL

