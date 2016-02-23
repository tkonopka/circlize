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



## helper function for updating an existing Rcss object
##
## -Rcss2        an existing Rcss object
## -le           factor levels
## -argslist     list of arg/values (e.g. from list(...))
## -args         character. On or more arguments (e.g. lwd)
## -selector     character. Used for updating the Rcss object
## -Rcssclass    the basic class, changes will be stored in subclasses
##
## == details
##
## This function updates an existing Rcss object.
## e.g. when user adds col=c(1,2,3), factors =c("a","b","c"), lwd=c(1,1,2)
## This function will introduce css definitions equivalent to 
## selector.a {col: 1; lwd: 1; }
## selector.b {col: 2; lwd: 1; }
## selector.c {col: 3; lwd: 2; }
##
updateRcss = function(Rcss2, le, argslist, args, selector, Rcssclass) {
  
  ## loop over arguments (e.g. lwd, lty, bg.col)
  for (k in seq_along(args)) {
    nowarg = args[k]
    if (nowarg %in% names(argslist)) {
      ## update data for this one argument
      temp = name.with.factors(argslist[[nowarg]], le)
      for (i in seq_along(temp)) {
        Rcss2 = RcssChangePropertyValue(Rcss2, selector=selector,
          Rcssclass=c(Rcssclass, names(temp)[i]),
          property=nowarg, value=temp[i])
      }      
    }    
  }
  
  return(Rcss2)
}




## Helper function to extract values for selected arguments
## from either Rcss objects or from an argument list (e.g. form list(...))
##
## -argslist     list of arguments (e.g. from list(...))
## -argsdefaults list with default values
## -Rcss         Rcss object
## -selector     selector in Rcss to use for lookup
## -Rcssclass    subclass
##
## -return
## A modified list similar to argslist.
## If the argslist has values specified for an argument (e.g. lwd),
## then the output list will have the same value as the input
## If the arglist does not have an argument specified,
## then the value is looked up from the Rcss object with
##
## The arguments processed in this way are those specified in the
## list args.defaults
##
RcssFromArgs = function(argslist, args.default, Rcss, selector, Rcssclass) {

  ans = argslist
  if (class(ans)!="list") {
    ans = list()
  }
  
  ## find names of arguments with required defaults but not in the argslist
  fillargs = names(args.default)
  fillargs = fillargs[!(fillargs %in% names(ans))]
  ## for arguments that require default but not in input argslist
  ## look up values from Rcss
  for (nowarg in fillargs) {
    ans[[nowarg]] = RcssGetPropertyValueOrDefault(Rcss, selector, nowarg, 
         default=args.default[[nowarg]], Rcssclass=Rcssclass)
  }
  
  ## return the modified arguments list
  return(ans)
}

