
setClass("GenericSeq",
         slots = c(id = "character", 
                   alphabet = "character", 
                   sequence =  "character"), 
         contains = "VIRTUAL",
         validity = function(object) {
             isValid <- TRUE                       # check if valid
             if (length(object@sequence) > 0) {
                 chars <- casefold(unique(unlist(strsplit(object@sequence, ""))))
                 # casefold same as tolower()
                 isValid <- all(chars %in% casefold(object@alphabet))    
             }
             if (!isValid)
                 cat("Some characters are not defined in the alphabet.\n")
             return(isValid)
         })


setClass("DnaSeq",
         contains = "GenericSeq",                     # subclass of GenericSeq
         prototype = list(
             id = paste("my DNA sequence", date()),
             alphabet = c("A","C","G","T"),
             sequence = character())
)

setClass("RnaSeq",
         contains = "GenericSeq",                      # subclass of GenericSeq
         prototype = list(
             id = paste("my RNA sequence", date()),
             alphabet = c("A","C","G","U"),
             sequence = character())
)



setGeneric("id", function(object, ...) standardGeneric("id"))
setGeneric("id<-", function(object,value) standardGeneric("id<-"))
setGeneric("alphabet", function(object, ...) standardGeneric("alphabet"))

## There is already a 'seq' method (see ?seq),
## although not a generic one (see isGeneric(seq))
setGeneric("seq", function(...) standardGeneric("seq"))
setGeneric("seq<-", function(object,value) standardGeneric("seq<-"))

## Same note that above for print
setGeneric("print", function(x, ...) standardGeneric("print"))

setGeneric("rev",function(x) standardGeneric("rev"))
setGeneric("comp",function(object, ...) standardGeneric("comp"))

setGeneric("transcribe", function(object, ...) standardGeneric("transcribe"))

setMethod("show", "GenericSeq",
          function(object) { 
              cat("Object of class", class(object),"\n")
              cat(" Id:", id(object), "\n")
              cat(" Length:", length(object), "\n")
              cat(" Alphabet:", alphabet(object), "\n")
              cat(" Sequence:", seq(object), "\n")
          })


setMethod("print", "GenericSeq",
          function(x) {
              sq <- strsplit(seq(x), "")[[1]]
              cat(">", id(x), "\n")
              cat(" 1   ")
              for (i in 1:length(x)) {
                  if ((i %% 10) == 0) {
                      cat("\n")
                      cat(i, "  ")
                  }
                  cat(sq[i])
              }
              cat("\n")
          })


setMethod("id", "GenericSeq", function(object, ...) object@id)

setMethod("id<-", "GenericSeq",
          function(object,value) object@id <- value)

setMethod("alphabet", "GenericSeq", function(object, ...) object@alphabet)
setMethod("length", "GenericSeq", function(x) nchar(x@sequence))
setMethod("seq", "GenericSeq", function(object, ...) object@sequence)

setReplaceMethod("id", signature(object = "GenericSeq", 
                                 value = "character"),
                 function(object, value) {
                     object@id <- value
                     if (validObject(object))
                         return(object)
                 })


setReplaceMethod("seq", signature(object = "GenericSeq", 
                                  value = "character"),
                 function(object, value) {
                     object@sequence <- value
                     if (validObject(object))
                         return(object)
                 })

setMethod("rev","GenericSeq",
          function(x) paste(rev(strsplit(seq(x), "")[[1]]), collapse = ""))


## this is only an example of initialize function,
## note Generic is in fact virtual          
setMethod("initialize", "GenericSeq",
          function(.Object, ..., id = "", sequence = ""){
              .Object@id <- id
              .Object@sequence <- toupper(sequence)
              callNextMethod(.Object, ...)
          })


setMethod("[", "GenericSeq",
          function(x, i, j = "missing", drop="missing") {
              if (any(i > length(x)))
                  stop("subscript out of bounds")
              s <- seq(x)
              s <- paste(strsplit(s,"")[[1]][i], collapse="")
              x@sequence <- s
              if (validObject(x))
                  return(x)
          })


setMethod("transcribe", "DnaSeq",
          function(object, ...) {
              .sequence <- chartr("T","U", toupper(seq(object)))
              .id <-  paste(id(object), "-- transcribed")
              rna <- new("RnaSeq",
                         id = .id,
                         alphabet = c("A","C","G","U"),
                         sequence = .sequence)
              return(rna)
          })


setMethod("comp", "DnaSeq",
          function(object, ...) {
              chartr("ACGT","TGCA", seq(object))
          })

setMethod("comp", "RnaSeq",
          function(object, ...) {
              chartr("ACGU","UGCA",seq(object))
          })



# constructors for DNA and RNA
DnaSeq <- function(id, sequence) 
    new("DnaSeq", id = id, sequence = sequence)

RnaSeq <- function(id, sequence) 
    new("RnaSeq", id = id, sequence = sequence)




