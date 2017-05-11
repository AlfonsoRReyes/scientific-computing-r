library(testthat)

source("./R/ode_generics.R")
source("./R/Kepler.R")
source("./R/RK45.R")
# source("./R/DormandPrince45.R")

r <- c(2, 0)
v <- c(0, 0.25)
dt <- 0.1

planet <- Kepler(r, v)

test_that("Have correct slot names", {
    expect_equal(slotNames(planet), c("GM", "state", "rate"))
})

test_that("Class is correct", {
    expect_true(class(planet) == "Kepler")
})

expect_equal(planet@state, c(2.00, 0.00, 0.00, 0.25, 0.00))
expect_equal(length(planet@rate), 0)
expect_equal(planet@GM, 1)

expect_equal(getState(planet), c(2.00, 0.00, 0.00, 0.25, 0.00))
state <- c(2.00, 0.00, 0.00, 0.25, 0.00)
rate <- vector("numeric", length(state)) # make a vector for rate
expect_equal(getRate(planet, state, rate), c(0.00, -0.25,  0.25,  0.00,  1.00))

# print(planet)
    test_that("rate is uninitialized even after calling getRate()", {
    expect_equal(planet@state, c(2.00, 0.00, 0.00, 0.25, 0.00))
    expect_equal(length(planet@rate), 0)
})

# now, we assign getRate to rate in object planet    
planet@rate <- getRate(planet, state, rate)    
planet@rate  

solver <- RK45(planet)
# solver <- DormandPrince45(planet)

solver@a
solver@b5
solver@er
solver@k

# print(planet)

# planet <- init(planet, initState = state)
# 
# test_that("match these values after init", {
#     expect_equal(getStepSize(planet@odeSolver), 0.01)
#     expect_equal(planet@odeSolver@ode@state, state)
#     expect_equal(planet@odeSolver@ode@rate, c(0, 0, 0, 0, 0))
#     expect_equal(planet@odeSolver@numEqn, 5)
# })
# 
# # run infinite loop. stop with ESCAPE.
# while (planet@state[5] <= 365) {
#     for (i in 1:5) {                 # advances time
#         planet <- doStep(planet)
#     }
#     # cat(sprintf("%12f %12f %12f %12f %12f \n", planet@state[1], planet@state[2], 
#     #             planet@state[3], planet@state[4], planet@state[5]))
# }
# 
# expect_equal(c(planet@state[1], planet@state[2], planet@state[3], 
#                planet@state[4], planet@state[5]), 
#              c(-1.657601,    -3.002031,     5.996665,    -0.407356,   365.050000 ))