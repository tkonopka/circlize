## ##################################################################
##
## This file is a complement to global.R
##
## This one contains variables and functions related to global variables,
## but using Rcssplot objects
##
## (Some functions from global.R do not need adjustments to work with
## Rcssplot. These functions are commented with no bodies ({ })
## as reminders of the stucture in global.R).
##
## ##################################################################



## .CIRCOS.ENV = new.env()

## resetGlobalVariable = function() { }

## resetGlobalVariable()

## circos.par = function(..., RESET = FALSE, READ.ONLY = NULL) {}
## circos.Rcsspar =  setGlobalOptions(...)

## is.circos.initialized = function() { }


# == title
# Initialize the circos layout
#
# == param
# -factors Factors which represent data categories
# -x       Data on x-axis, a vector
# -xlim    Limitations for values on x-axis
# -sector.width Width for each sector. The length of the vector should be either 1 which means
#          all sectors have same width or as same as the number of sectors. Values for
#          the vector are relative, and they will be scaled by dividing their summation.
#          By default, it is ``NULL`` which means the width of sectors correspond to the data
#          range in sectors which is calculated internally.
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# The function allocates the sectors according to the values on x-axis.
# The number of sectors are determined by the ``factors`` and the order
# of sectors are determined by the levels of factors. In this function,
# the start and end position for each sector on the circle (measured by degree)
# are calculated according to the values on x-axis.
#
# If ``x`` is set, the length of ``x`` must be equal to the length of ``factors``.
# Then the data range for each sector are calculated from ``x`` and ``factors``.
#
# If ``xlim`` is set, it should be a vector containing two numbers or a matrix with 2 columns.
# If ``xlim`` is a 2-element vector, it means all sector share the same ``xlim``.
# If ``xlim`` is a 2-column matrix, the number of rows should be equal to the number of categories (number of levels)
# identified by ``factors``, then each row of ``xlim`` corresponds to the data range for each sector
# and the order of rows is corresponding to the order of levels of ``factors``.
#
# Normally, width of sectors will be calculated internally according to the data range in sectors. But you can
# still set the width manually. However, it is not always a good idea to change the default sector width since
# the width can reflect the range of data in sectors. Anyway, in some cases, it is useful to manually set
# the width such as you want to zoom in some part of the sectors.
#
# The function finally calls `graphics::plot` and be ready for adding graphics.
#
# Settings from selector "circlize" in Rcss object: cell.padding, gap.degree, start.degree, clock.wise, canvas.xlim, canvas.ylim
#
circos.Rcssinitialize = function(factors, x = NULL, xlim = NULL, sector.width = NULL, Rcss="default", Rcssclass=c(), ...) {
  
  resetGlobalVariable()
  
  ## extract Rcss values
  circlize.default = list(start.degree=0, clock.wise=TRUE, unit.circle.segments=200,
    canvas.xlim=c(-1,1), canvas.ylim=c(-1,1), bg.col=NA)
  region.default = list(gap.degree=1, cell.padding=c(0.02, 1, 0.02, 1))
  args = list(...)
  args = RcssFromArgs(args, circlize.default, Rcss, "circlize", Rcssclass)
  args = RcssFromArgs(args, region.default, Rcss, "circlizeregion", Rcssclass)
  cell.padding = args$cell.padding
  gap.degree =args$gap.degree
  start.degree = args$start.degree;
  clock.wise = args$clock.wise;  
  unit.circle.segments = args$unit.circle.segments;

  ## end of Rcss lookup
  
  .SECTOR.DATA = get(".SECTOR.DATA", envir = .CIRCOS.ENV)
  .CELL.DATA = get(".CELL.DATA", envir = .CIRCOS.ENV)
  
  if(any(factors == "")) {
    stop("`factors` cannot contain empty strings.\n")
  }
  
  if(! is.factor(factors)) {
    factors = factor(factors)
  }
  le = levels(factors)
  
  ## initialize .SECTOR.DATA
  ## you can think it as the global x axis configuration
  ## calculate min and max value for each sectors
  ## there are several ways
  ## xlim is prior than x
  if(is.vector(xlim)) {
    if(length(xlim) != 2) {
      stop("Since `xlim` is vector, it should have length of 2.\n")
    }    
    
    min.value = rep(xlim[1], length(le))
    max.value = rep(xlim[2], length(le))
  } else if(is.matrix(xlim)) {
    if(dim(xlim)[1] != length(le) || dim(xlim)[2] != 2) {
      stop("Since `xlim` is a matrix, it should have same number of rows as the length of the level of `factors` and number of columns of 2.\n")
    }
    
    min.value = apply(xlim, 1, function(x) x[1])
    max.value = apply(xlim, 1, function(x) x[2])
  } else if(is.vector(x)) {
    
    if(length(x) != length(factors)) {
      stop("Length of `x` and length of `factors` differ.\n")
    }
    min.value = tapply(x, factors, min)
    max.value = tapply(x, factors, max)
  } else {
    stop("You should specify either `x` or `xlim`.\n")
  }
  
  ## range for sectors
  sector.range = max.value - min.value
  n.sector = length(le)
  
  sector = vector("list", 7)
  ## for each sector, `start.degree always referto `min.value` and `end.degree` always
  ## refer to `max.value` in a reverse clockwise fasion. So here `start.degree` and 
  ## `end.degree` also correspond to the direction.
  ## So in the polar coordinate, `start.degree` would be larger than `end.degree`
  names(sector) = c("factor", "min.value", "max.value", "start.degree", "end.degree", "min.data", "max.data")
  sector[["factor"]] = le
  sector[["min.data"]] = min.value
  sector[["max.data"]] = max.value
  
  if(length(gap.degree) == 1) {
    gap.degree = rep(gap.degree, n.sector)
  } else if(length(gap.degree) != n.sector) {
    stop("Since `gap.degree` parameter has length larger than 1, it should have same length as number of levels of factors.\n")
  }
  
  circos.par("unit.circle.segments"=unit.circle.segments)
  
  if(360 - sum(gap.degree) <= 0) {
    stop("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.\n")
  }
  
  if(is.null(sector.width)) {
    ## degree per data
    unit = (360 - sum(gap.degree)) / sum(sector.range)
    
    for(i in seq_len(n.sector)) {
      
      if(sector.range[i] == 0) {
        stop(paste("Range of the sector (", le[i] ,") cannot be 0.\n", sep = ""))
      }
      
      ## only to ensure value are always increasing or decreasing with the absolute degree value
      if(clock.wise) {
        sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
        sector[["end.degree"]][i] =  sector[["start.degree"]][i] - sector.range[i]*unit
      } else {
        sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
        sector[["start.degree"]][i] = sector[["end.degree"]][i] + sector.range[i]*unit   
      }
      }
  } else {
    if(length(sector.width) == 1) {
      sector.width = rep(sector.width, n.sector)
    } else if(length(sector.width) != n.sector) {
      stop("Since you manually set the width for each sector, the length of `sector.width` should be either 1 or as same as the number of sectors.\n")
    }
    
    sector.width.percentage = sector.width / sum(sector.width)
    degree.per.sector = (360 - sum(gap.degree)) * sector.width.percentage
    
    if(any(degree.per.sector <= 0)) {
      stop("Maybe your `gap.degree` is too large so that there is no space to allocate sectors.\n")
    }
    
    for(i in seq_len(n.sector)) {
      
      if(sector.range[i] == 0) {
        stop(paste("Range of the sector (", le[i] ,") cannot be 0.\n", sep = ""))
      }
      
      ## only to ensure value are always increasing or decreasing with the absolute degree value
      if(clock.wise) {
        sector[["start.degree"]][i] = ifelse(i == 1, start.degree, sector[["end.degree"]][i-1] - gap.degree[i-1])
        sector[["end.degree"]][i] =  sector[["start.degree"]][i] - degree.per.sector[i]
      } else {
        sector[["end.degree"]][i] = ifelse(i == 1, start.degree, sector[["start.degree"]][i-1] + gap.degree[i-1])
        sector[["start.degree"]][i] = sector[["end.degree"]][i] + degree.per.sector[i] 
      }
    }
  }
  ## from start.degree, degree is increasing in a reverse-clock wise fasion
  ## so, if circos is created clock wise, the forward sector would have large degrees
  ## if circos is created reverse clock wise, the forward sector would have small degrees
  ## just for goodlooking for the degree
  if(clock.wise) {
    sector[["start.degree"]] = sector[["start.degree"]] + 360
    sector[["end.degree"]] = sector[["end.degree"]] + 360
  }
    
  if(any(cell.padding[2] + cell.padding[4] >= sector[["start.degree"]] - sector[["end.degree"]])) {
    stop("Summation of cell padding on x-direction are larger than the width for some sectors.\nYou can set 'cell.padding: 0.02 0 0.02 0;' in the circlizeregions selector or remove tiny sectors.\n")
  }
  
  min.value = min.value - cell.padding[2]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real min value
  max.value = max.value + cell.padding[4]/(sector[["start.degree"]] - sector[["end.degree"]] - cell.padding[2] - cell.padding[4])*sector.range  # real max value
  sector[["min.value"]] = min.value
  sector[["max.value"]] = max.value
  
  sector = as.data.frame(sector, stringsAsFactors = FALSE)
  .SECTOR.DATA = sector
  
  ## initialize .CELL.DATA which contains information of each cell
  ## if content of that cell has been created, it means that the 
  ## plotteing region for that cell has been created.
  .CELL.DATA = vector("list", length = length(le))
  names(.CELL.DATA) = le
  for(i in seq_along(.CELL.DATA)) {
    .CELL.DATA[[ le[i] ]] = vector("list", length = 0)
  }
  
  assign(".SECTOR.DATA", .SECTOR.DATA, envir = .CIRCOS.ENV)
  assign(".CELL.DATA", .CELL.DATA, envir = .CIRCOS.ENV)
  
  ## draw everything in a unit circle
  Rcssplot(args$canvas.xlim, args$canvas.ylim, type = "n", ann = FALSE, axes = FALSE, Rcss=Rcss, Rcssclass=Rcssclass)
  if (!identical(args$bg.col, NULL)) {
    parusr = par("usr")
    Rcssrect(parusr[1], parusr[3], parusr[2], parusr[4],
             col=args$bg.col, border=NA, lwd=0, 
             Rcss=Rcss, Rcssclass=c(Rcssclass, "canvas"))
  }
  
  ## all the information of cells would be visited through `get.cell.meta.data`
  return(invisible(NULL))
}

## circos.clear = function() { ... }

## get.all.sector.index = function() { ...}

## get.all.track.index = function() { ... }

## get.sector.data = function(sector.index = get.current.sector.index()) { ... }

## get.current.track.index = function() { ... }

## set.current.track.index = function(x) { ... }

## get.current.sector.index = function() { ...}

## set.current.sector.index = function(x) { ... }

## get.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index()) { ... }

## set.cell.data = function(sector.index = get.current.sector.index(), track.index = get.current.track.index(), ...) { ... }

## has.cell = function(sector.index, track.index) { ... }

## circos.info = function(sector.index = NULL, track.index = NULL, plot = FALSE) { ... }

## show.index = function() { ... }

## get.cell.meta.data = function(name, sector.index = get.current.sector.index(), 
##                               track.index = get.current.track.index()) { ... }


