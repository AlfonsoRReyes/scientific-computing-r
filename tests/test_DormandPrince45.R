library(testthat)

source("./R/ODE.R")
source("./R/DormandPrince45.R")

DormandPrince45 <- function(ode) {
    
    new("DormandPrince45", ode)
}

ode = new("ODE")
dprince45 <- DormandPrince45(ode)

# expect_true()
expect_true(class(dprince45) == "DormandPrince45")

# print(slotNames(dprince45))
# print(dprince45@a)

expect_equal(slotNames(dprince45),
c("error_code",                 "a",                         "b5",
   "er",                        "numStages",                 "stepSize",                 
   "numEqn",                    "temp_state",                "k",                        
   "truncErr",                  "ode",                       "tol",                      
   "enableExceptions",          "NO_ERROR",                  "DID_NOT_CONVERGE",         
   "BISECTION_EVENT_NOT_FOUND")
)

expect_equal(c( dprince45@numStages, 
                dprince45@stepSize,
                dprince45@numEqn,
                dprince45@tol,
                dprince45@enableExceptions),
                c(6, 0.01, 0, 1e-06, FALSE)
                )

expect_equal(dprince45@b5,
             c(19.0/216.0, 0.0, 1000.0/2079.0, -125.0/216.0, 81.0/88.0, 5.0/56.0)
)

expect_equal(dprince45@er,
             c(-11.0/360.0, 0.0, 10.0/63.0, -55.0/72.0, 27.0/40.0, -11.0/280.0)
)

dprince45 <- init(dprince45, 0.123)
cat(dprince45@stepSize)
cat(dprince45@numEqn)
print(dprince45@temp_state)
print(dprince45@k)