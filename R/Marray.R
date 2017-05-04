

setClass("MArray",
                   slots = c(marray = "matrix",
                             fmeta = "data.frame",
                             pmeta = "data.frame")
)



# Accessors
setGeneric("marray", function(object, ...) standardGeneric("marray"))
setGeneric("fmeta", function(object, ...) standardGeneric("fmeta"))
setGeneric("pmeta", function(object, ...) standardGeneric("pmeta"))
setGeneric("getValuesAtProbeForCondition", function(object, ...) 
    standardGeneric("getValuesAtProbeForCondition"))
setGeneric("getSampleConditions", function(object, ...) 
    standardGeneric("getSampleConditions"))
setGeneric("getProbePathways", function(object, ...)
    standardGeneric("getProbePathways"))
setGeneric("getValuesAtSampleForPathway", function(object, ...)
    standardGeneric("getValuesAtSampleForPathway"))



setMethod("marray", "MArray", function(object) object@marray)

setMethod("pmeta", "MArray", function(object) object@pmeta)

setMethod("fmeta", "MArray", function(object) object@fmeta)

# show doesn't need a generic; already is
setMethod("show",
          signature = "MArray",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep = "")
              cat(" ", nrow(object@marray), " features by ", ncol(object@marray),
                  " samples.\n", sep = "")
              #invisible(NULL)
          })

setMethod("[", "MArray",
          function(x, i, j, drop = "missing") {
              .marray <- x@marray[i, j]
              .pmeta  <- x@pmeta[j, ]
              .fmeta  <- x@fmeta[i, ]
              MArray(marray = .marray,
                     fmeta  = .fmeta,
                     pmeta  = .pmeta)
          })

setMethod("summary", "MArray", 
          function(object, sample) {
              summary(object@marray[, sample])
          })


setMethod("getValuesAtProbeForCondition", "MArray",
          function(object, probe, condition) {
              cond = object@pmeta[, "condition"] == condition
              return(object@marray[probe, cond])
          })

setMethod("getValuesAtSampleForPathway", "MArray",
          function(object, sample, pathway) {
              cond = object@fmeta[, "pathway"] == pathway
              return(object@marray[cond, sample])
          })


setMethod("getSampleConditions", "MArray", 
          function(object) {
              unique(object@pmeta$condition)
          })


setMethod("getProbePathways", "MArray", 
          function(object) {
              unique(object@fmeta$pathway)
          })


# constructor
MArray <- function(marray, fmeta, pmeta) {
    new("MArray", marray = marray, fmeta = fmeta, pmeta = pmeta)
}