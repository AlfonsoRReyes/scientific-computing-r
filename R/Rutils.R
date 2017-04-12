#' Take objects and create a list using their names
#' 
named.list <- function(...) { 
  nl <- setNames( list(...) , as.character( match.call()[-1]) )
  # nl <- setNames( list(...) , as.character( match.call()[-1]) )  
  nl
}
