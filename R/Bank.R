
setClass("Bank", slots = c(cashPoints = "list"))

# the Bank constructor reads a CSV file with the ATM id's and the
# type of CashPoint
setMethod("initialize", "Bank", 
    function(.Object) { 
      # The Bank Model is a dataframe of cash points id and type
      BankModel <- read.table(file = "./inst/extdata/bank_model.csv",
                              sep = ";", header = TRUE, stringsAsFactors = FALSE)
      .Object@cashPoints <- apply(BankModel, 1, 
                                  function(cp) {
                                      # cp[2]: ATM or Branch; cp[1]: id
                                      cat(sprintf("%-8s %-14s \n", cp[2], cp[1]))
                                      # New instance of Branch or ATM 
                                      #        new(ATM, "CashPoint_#")      or
                                      #        new(Bank, "CashPoint_#")
                                      new(cp[2], cp[1]) })  # get balance by id
      # add a name to each list. One list per CashPoint (date and transactions)
      # Example: CashPoint_1_ATM, CashPoint_2_Branch
      #    combining column1 and column2 of the BankModel dataframe
      names(.Object@cashPoints) <- apply(BankModel, 1, paste, collapse = "_")
      return(.Object)
    })

setGeneric("givePrediction", function(object) {
    standardGeneric("givePrediction")
})

setMethod("givePrediction", "Bank", function(object){
    # ocp <<- object@cashPoints
    return(sapply(object@cashPoints, "givePrediction"))
})





# CashPoint has two slots: id and balances
setClass("CashPoint", 
         slots = c( id = "character", 
                    balances = "data.frame"), 
         contains = "VIRTUAL")

# CashPoint doesn't do anything; only lends structure to ATM or Branch
setMethod("initialize", "CashPoint", 
          function(.Object, cashPointId) { 
              .Object@id <- cashPointId 
              balances <- read.table(file = "./inst/extdata/branch_balances_data.csv",
                                     sep = ";", header = TRUE)
              # bal <<- balances
              .Object@balances <- subset(balances, # get balance by id
                                         balances$BranchId == .Object@id, -BranchId) 
              # obal <<-  .Object@balances
              .Object@balances$Date <- as.Date(.Object@balances$Date) # convert to date
              return(.Object)
          })

setMethod("givePrediction", "CashPoint", function(object){
    stop("no givePrediction method for this class")
})





setClass("Branch", contains = "CashPoint")           # inherits from CashPoint

setMethod("givePrediction", "Branch", 
          function(object){
              # for Branch only take the mean
              return(mean(object@balances$Balance))
          })



setClass("ATM", contains = "CashPoint")              # inherits from CashPoint

setMethod("givePrediction", "ATM", 
          function(object) { 
              LM <- lm(Balance ~ as.numeric(Date), data = object@balances) 
              prediction <- predict(LM, data.frame(Date = 1 + max(object@balances$Date)))
              return(unname(prediction))
          })



