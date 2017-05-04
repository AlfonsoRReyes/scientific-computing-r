

Customer <- setClass("Customer", 
                     slots = c(
                         CustomerID = "numeric", 
                         Name = "character", 
                         OrderHistory = "list"), 
                     prototype = list(
                         OrderHistory = list()))

Order <- setClass(Class="Order", 
                  slots = c(
                      Description = "character", 
                      Cost = "numeric"))

setGeneric("add<-", function(object, value, ...) StandardGeneric("add<-"))

setMethod(f = "add<-", 
          signature = c("Customer", "Order"), 
          definition = function(object, value) {
              object@OrderHistory <- append(object@OrderHistory, value)
              object    
          })

setMethod(f = "show", 
          signature = "Customer", 
          definition = function(object) { 
              cat("** Customer #", object@CustomerID, ": ", object@Name, "\n", sep="")
              for(i in object@OrderHistory) 
                  cat("\t", i@Description, "\t", i@Cost, "\n", sep="")
          })


## Customers -- analogous to a data.frame or data base table
setClass(Class = "Customers", slots = c(
    CustomerId = "integer", 
    Name = "character"))

## Items -- analogous to a data.frame or data base table
setClass(Class = "Items", slots = c(
                    ItemId = "integer", 
                    Description = "character", 
                    Cost = "numeric"))

## Transactions -- analogous to a data.frame or data base table
setClass(Class = "Transactions", slots = c(
                    TransactionId = "integer", 
                    CustomerId = "integer", 
                    ItemId = "integer"))




## Business -- analogous to a data *base*
Business = setClass(Class = "Business", slots = c(
                    Customers = "Customers",          # use class `Customers`
                    Items = "Items",                  # use class `Items`
                    Transactions = "Transactions"))   # use class `Transactions`




# For a little completeness, here's a minimal implementation starting with 
# some utility functions for generating sequential IDs and for updating object slots
# .nextid: increases the identifier for any slot
# .update: 

.nextid <- function(x, slotName, n = 1L)
    max(0L, slot(x, slotName)) + seq_len(n)

.update <- function(x, ...) {
    args <- list(...)
    for (nm in names(args))
        args[[nm]] <- c(slot(x, nm), args[[nm]])
    do.call("initialize", c(list(x), args))
}



# The following functions add vectors of customers and items to the business
add_customers <- function(business, customerNames)
{
    customers <- slot(business, "Customers")
    len <- length(customerNames)
    initialize(business,
               Customers = .update(customers, 
                                   CustomerId = .nextid(customers, "CustomerId", 
                                                        len), 
                                   Name = customerNames))
}

add_items <- function(business, descriptions, costs)
{
    items <- slot(business, "Items")
    len <- length(descriptions)
    initialize(business,
               Items = .update(items, 
                               ItemId = .nextid(items, "ItemId", len), 
                               Description = descriptions, Cost=costs))
}



.purchase <- function(business, customerId, itemIds)
{
    transactions <- slot(business, "Transactions")
    len <- length(itemIds)
    initialize(business,
               Transactions = .update(transactions, 
                                      TransactionId = rep(.nextid(transactions, 
                                                                  "TransactionId"), 
                                                          len), 
                                      CustomerId = rep(customerId, len), 
                                      ItemId = itemIds))
}

