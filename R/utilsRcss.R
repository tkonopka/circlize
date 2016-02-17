## some custom "util" functions used within the Rcss functions
##



## assign names to a vector (if vector has names already, leave them alone)
name.with.factors = function(x, le) {
  if (is.null(names(x))) {
    x = rep(x, length(le))[1:length(le)]
    names(x) = le;
    return(x)
  }
  return(x)
}


