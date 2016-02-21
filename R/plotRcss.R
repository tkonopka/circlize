## ##################################################################
##
## This is part1 or plotRcss (circlize package)
## This part contains functions for basic graphics (points, lines, etc)
##
## ##################################################################


# == title
# Create plotting regions for a whole track
#
# == param
# -factors      Factors which represent categories of data, if it is ``NULL``, 
#               then it uses the whole sector index.
# -x            Data on x-axis. It is only used if ``panel.fun`` is set.
# -y            Data on y-axis
# -ylim         Range of data on y-axis
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``. Normally,
#               all cells on a same track should have same ``ylim``.
# -track.index  Index for the track which is going to be created/updated. If the specified track has already
#               been created, this function just updated corresponding track with new plot. If the specified track
#               is ``NULL`` or has not been created, this function just created it. Note the value for this
#               argument should not exceed maximum track index plus 1.
# -panel.fun    Panel function to add graphics in each cell, see "details" section
#               and vignette for explanation.
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# This function pretends to be a high-level plotting function, which means, 
# you must first call this function to create plotting regions, then those
# low-level graphical function such as `circos.points`, `circos.lines` can be
# applied.
#
# It has two different usages. First, it can create a complete track which among several
# sectors. Because currently it does not support creating single cell since it will
# make the layout disordered, this is the only way to create plotting regions.
#
# Currently, all the cells that are created in a same track sharing same height, which means,
# there is no cell has larger height than others.
#
# Since limitation for values on x-axis has already been defined by `circos.initialize`, only
# limitation for values on y-axis should be specified in this function. The ``x`` argument is only
# used if you set ``panel.fun``. There are two ways to identify the limitation for values on y-axes either by ``y``
# or ``ylim``. If ``y`` is set, it must has the same length as ``factors`` and the ``ylim`` for each cell is calculated
# from y values. Also, the ylim can be specified from ``ylim`` which can be a two-element vector or a matrix which
# has two columns and the number of rows is the same as the length of the levels of the factors.
#
# If there is no enough space for the new track or the new track has overlap with other tracks,
# there will be an error.
#
# ``panel.fun`` provides a convenient way to add graphics in each cell when initializing the 
# tracks. The self-defined function need two arguments: ``x`` and ``y`` which correspond to the data points
# in the current cell. `circos.trackPlotRegion` creates plotting regions one by one on the track and
# ``panel.fun`` adds graphics in the 'current' cell after the plotting region for a certain cell has been
# created. See vignette for examples of how to use this feature.
#
# If ``factors`` does not cover all sectors, the cells in remaining unselected
# sectors would also be created but without drawing anything. The ``ylim`` for these cells
# are the same as that in the latest created cell.
#
# Second, it can update a already-created track if the index for the track
# is specified. If the index is one larger than the largest track index, it in fact
# creates the new track. If updating an existed track, those parameters related to the position (such as track height and track margin)
# of the plotting region can not be changed.
#
# Settings from selector "circlize" in Rcss object:
#  track.height, track.margin, cell.padding,
# Settings from selector "circlizeregions" in Rcss object:
#  bg.border, bg.col, bg.lty, bg.lwd
# Arguments in "..." processed by Rcssplot: 
#
circos.RcsstrackPlotRegion = function(factors = NULL, x = NULL, y = NULL, ylim = NULL,
  force.ylim = TRUE, track.index = NULL,
  panel.fun = function(x, y) {NULL},
  Rcss="default", Rcssclass=c(), ...) {
  
  if(!is.circos.initialized()) {
    stop("Your circos plot has not been initialized yet!\n")
  }

  ## get some info on tracks
  args = list(...)
  track.args = c("track.height", "track.margin", "cell.padding")
  track.height = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "track.height", Rcssclass=Rcssclass, default=0.2)
  track.margin = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "track.margin", Rcssclass=Rcssclass, default=c(0.01, 0.01))
  o.track.margin = track.margin;
  cell.padding = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "cell.padding", Rcssclass=Rcssclass, default=c(0.02, 1, 0.02, 1))
  o.cell.padding = cell.padding;
  args = args[!(names(args) %in% track.args)]

  ## if there is no factors, default are all the available factors
  if(is.null(factors)) {
    factors = get.all.sector.index()
    factors = factor(factors, levels = factors)
  }
  
  ## although ``x`` and ``y`` are not necessary, but once they are set, they must
  ## have same length as ``factors``
  if(!is.null(y) && length(y) != length(factors) ||
     !is.null(x) && length(x) != length(factors)) {
    stop("Length of data and length of factors differ.\n")
  }
  
  ## need to be a factor
  if(!is.factor(factors)) {
    factors = factor(factors)
  }
  
  ## check whether there are some categories that are not in the circle
  setdiff.factors = setdiff(levels(factors), get.all.sector.index())
  if(length(setdiff.factors)) {
    stop(paste("Cannot find these categories in existing sectors:", paste(setdiff.factors, collapse = ", "), ".\n", sep = ""))
  }
  
  tracks = get.all.track.index()
  last.track.index = ifelse(length(tracks), tracks[length(tracks)], 0)
  flag_createNewTrack = 0
  if(is.null(track.index)) {
    ## new track should inside the most recently created track
    set.current.track.index(last.track.index + 1)
    track.index = get.current.track.index()
    flag_createNewTrack = 1
  } else if(track.index == last.track.index + 1) {
    ## if the track.index is next to the most recently created track
    set.current.track.index(track.index)
    track.index = get.current.track.index()
    flag_createNewTrack = 1
  } else {   
    if(track.index > last.track.index + 1) {
      stop(paste("Wrong track index: it should be no more than ", last.track.index + 1, "\n", sep = ""))
    }
    ## update an existed track
    if(track.index <= tracks[length(tracks)]) {
      ## ignore track.height from args
      track.height = get.cell.meta.data("track.height", sector.index = factors[1], track.index = track.index)
      ## ignore track.margin 
      circos.par("track.margin" = get.cell.meta.data("track.margin", sector.index = factors[1], track.index = track.index))
    }
    if(is.null(ylim) && is.null(y)) {
      for(sid in get.all.sector.index()) {
        ylim = rbind(ylim, get.cell.meta.data("ylim", sector.index = sid, track.index = track.index))
      }
    }
    set.current.track.index(track.index)
  }
    
  le = levels(factors)
  nlevel = length(le)

  ## perhaps transfer some sector-specific graphics args from arguments into update Rcss
  graphics.args = c("bg.col", "bg.border", "bg.lty", "bg.lwd")
  Rcss2 = updateRcss(Rcss, le, args, graphics.args, "circlizeregion", Rcssclass)
  args = args[!(names(args) %in% graphics.args)]
  ## end of Rcss object update
  
  ## whether to force ylim for all cells in a track same
  if(is.null(ylim)) {
    if(is.null(y)) {
      stop("You have to specify either `y` or `ylim`.\n")
    }
    
    if(force.ylim) {
      y.range = range(y)
      y.range = matrix(rep(y.range, nlevel), ncol = 2, byrow = TRUE)
    } else {
      y.range = tapply(y, factors, range)
      y.range = matrix(unlist(y.range), ncol = 2, byrow = TRUE)
    }
  }

  if(flag_createNewTrack) {
    if(track.index == 1) {
      track.start = 1 - o.track.margin[2];
    } else {
      track.start = get.cell.meta.data("cell.bottom.radius", track.index = track.index - 1) - 
        get.cell.meta.data("track.margin", track.index = track.index - 1)[1] -
          o.track.margin[2]      
    }
  } else {
    track.start = get.cell.meta.data("cell.top.radius", track.index = track.index)
  }

  ## check whether there is enough space for the new track and whether the new space
  ## overlap with other tracks. Only for creatation mode.
  if(flag_createNewTrack) {
    check.track.position(track.index, track.start, track.height)
  }
  
  ## if `ylim` is specified
  if(!is.null(ylim)) {
    if(is.vector(ylim) && length(ylim) == 2) {
      ylim = matrix(rep(ylim, length(le)), ncol = 2, byrow = TRUE)
    } else if(is.matrix(ylim) && ncol(ylim) == 2 && nrow(ylim) == length(le)) {
      
    } else {
      stop("Wrong `ylim` format.\n")
    }
  } 

  ## now for each factor, create plotting region
  for(i in seq_along(le)) {
    ## `ylim` is prior to `y`
    if(is.null(ylim)) {
      ylim2 = y.range[i, ]
    } else {
      ylim2 = ylim[i, ]
    }
    
    ## create plotting region for single cell
    do.call(circos.RcsscreatePlotRegion,
            c(list(track.start = track.start, track.height = track.height, sector.index = le[i],
                   track.index = track.index, ylim = ylim2, 
                   Rcss=Rcss2, Rcssclass=c(Rcssclass, le[i])), args))
    
    l = factors == le[i]
    if(!is.null(panel.fun)) {
      if(is.null(x)) {
        nx = NULL
      } else {
        nx = x[l]
      }
      
      if(is.null(y)) {
        ny = NULL
      } else {
        ny = y[l]
      }
      panel.fun(nx, ny)
    }
    
  }
  
  ## and those sectors not include in factors
  le2 = setdiff(get.all.sector.index(), levels(factors))
  if(length(le2)) {
    for(i in seq_along(le2)) {
      ## ylim is the most recent ``ylim2``
      do.call(circos.RcsscreatePlotRegion,
              c(list(track.start = track.start, track.height = track.height, sector.index = le2[i],
                      track.index = track.index, ylim = ylim2, bg.col = NA, bg.border = NA,
                      Rcss=Rcss, Rcssclass=Rcssclass), args))     
    }
  }
  
  return(invisible(NULL))  
}



# == title
# Create plotting regions for a whole track
#
# == param
# -... pass to `circos.RcsstrackPlotRegion`
#
# == details
# shortcut function of `circos.RcsstrackPlotRegion`.
#
circos.Rcsstrack = function(...) {
	circos.RcsstrackPlotRegion(...)
}



# == title
# Update the plotting region in an existed cell
#
# == param
# -sector.index Index for the sector
# -track.index  Index for the track
# -bg.col       Background color for the plotting region
# -bg.border    Color for the border of the plotting region
# -bg.lty       Line style for the border of the plotting region
# -bg.lwd       Line width for the border of the plotting region
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# You can update an existed cell by this function by erasing all the graphics.
# But the ``xlim`` and ``ylim`` inside the cell still remains unchanged. 
#
# Note if you use `circos.trackPlotRegion` to update an already created track, you can re-define ``ylim`` in these cells.
#
# Arguments in "..." processed by circlize: bg.border, bg.col, bg.lty, bg.lwd
# Arguments in "..." processed by Rcssplot: 
#
circos.RcssupdatePlotRegion = function(sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"),  
  Rcss="default", Rcssclass=c(), ...) {
  
  if(!has.cell(sector.index, track.index)) {
    stop("You can only update an existed cell.\n")
  }

   ## start lookup with Rcss
  args = list(...);
  if (!hasArg(bg.col)) {
    bg.col = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.col", Rcssclass=c(Rcssclass, sector.index), default="#ffffff")
  } else {
    bg.col = args[["bg.col"]]
  }
  if (!hasArg(bg.border)) {
    bg.border = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.border", Rcssclass=c(Rcssclass, sector.index), default=NA)
  } else {
    bg.border = args[["bg.border"]]
  }  
  if (!hasArg(bg.lty)) {
    bg.lty = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.lty", Rcssclass=c(Rcssclass, sector.index), default=1)
  } else {
    bg.lty = args[["bg.lty"]]
  }    
  if (!hasArg(bg.lwd)) {
    bg.lwd = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.lwd", Rcssclass=c(Rcssclass, sector.index), default=1)
  } else {
    bg.lwd = args[["bg.lwd"]]
  }
  args = args[!(names(args) %in% c("bg.col","bg.border","bg.lty", "bg.lwd"))]
  ## end lookup with Rcss
  
  cell.xlim = get.cell.meta.data("cell.xlim", sector.index = sector.index, track.index = track.index)
  cell.ylim = get.cell.meta.data("cell.ylim", sector.index = sector.index, track.index = track.index)
  
  set.current.sector.index(sector.index)
  set.current.track.index(track.index)
  
  ## cover the exsited region by fill with white
  lwd = get.cell.meta.data("bg.lwd", sector.index = sector.index, track.index = track.index)
  circos.Rcssrect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], 
                  col = "white", border = "white", lty = 1, lwd = lwd, Rcss=Rcss, Rcssclass=Rcssclass)
  circos.Rcssrect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], 
                  col = bg.col, border = bg.border, lty = bg.lty, lwd = bg.lwd, Rcss=Rcss, Rcssclass=Rcssclass)
  return(invisible(NULL))
}



# == title
# Create plotting regions for a whole track
#
# == param
# -... pass to `circos.RcssupdatePlotRegion`
#
# == details
# shortcut function of `circos.RcssupdatePlotRegion`.
#
circos.Rcssupdate = function(...)  {
  circos.RcssupdatePlotRegion(...)
}



# internal, so we do not need to check arguments
## the track.height=circos.par() would be thing to eliminate (but since this function is only used internally,
## for now can just check if track.height is always set when this function is called)
circos.RcsscreatePlotRegion = function(track.start, track.height = circos.par("track.height"),
  sector.index = get.cell.meta.data("sector.index"), track.index = get.cell.meta.data("track.index"), ylim,  
  Rcss="default", Rcssclass=c(), ...) {
  
  ## we do not have such meta for the cell, so we need to calculate them
  sector.data = get.sector.data(sector.index)
  cell.xlim = c(sector.data["min.value"], sector.data["max.value"])
  names(cell.xlim) = NULL
  
  cell.padding = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "cell.padding", Rcssclass=Rcssclass, default=c(0.02, 1, 0.02, 1))
  track.margin = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "track.margin", Rcssclass=Rcssclass, default=c(0.01, 0.01))
  
  ## get graphics parameters from Rcss
  args = list(...);
  
  if (!hasArg(bg.col)) {
    bg.col = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.col",
      default=NA, Rcssclass=Rcssclass)
  } else {
    bg.col = args[["bg.col"]]
  }
  if (!hasArg(bg.border)) {
    bg.border = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.border",
      default="#000000", Rcssclass=Rcssclass)
  } else {
    bg.border = args[["bg.border"]]
  }
  if (!hasArg(bg.lty)) {
    bg.lty = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.lty",
      default=1, Rcssclass=Rcssclass)
  } else {
    bg.lty = args[["bg.lty"]]
  }
  if (!hasArg(bg.lwd)) {
    bg.lwd = RcssGetPropertyValueOrDefault(Rcss, "circlizeregion", "bg.lwd",
      default=1, Rcssclass=Rcssclass)
  } else {
    bg.lwd = args[["bg.lwd"]]
  }
  ## end of Rcss fetch
        
  xlim = c(sector.data["min.data"], sector.data["max.data"])
  
  if(cell.padding[1] + cell.padding[3] >= track.height) {
    stop("Summation of cell padding on y-direction are larger than the height of the cells.\n")
  }
  
  yl = numeric(2)
  yl[1] = ylim[1] - (ylim[2] - ylim[1])*cell.padding[1] / track.height
  yl[2] = ylim[2] + (ylim[2] - ylim[1])*cell.padding[3] / track.height
  
  set.cell.data(sector.index = sector.index,
                track.index = track.index,
		xlim = xlim,
		ylim = ylim,
                cell.xlim = cell.xlim,
                cell.ylim = yl,
                track.start = track.start,
                track.height = track.height,
		track.margin = track.margin,
		cell.padding = cell.padding,
                bg.col = bg.col,
                bg.border = bg.border,
                bg.lty = bg.lty,
                bg.lwd = bg.lwd)
  
  set.current.sector.index(sector.index)
  
  ## The plotting region is a rectangle
  cell.ylim = yl
  circos.Rcssrect(cell.xlim[1], cell.ylim[1], cell.xlim[2], cell.ylim[2], sector.index = sector.index, track.index = track.index,
                  col = bg.col, border = bg.border, lty = bg.lty, lwd = bg.lwd, Rcss=Rcss, Rcssclass=Rcssclass)
  return(invisible(NULL))
}



# == title 
# Add points to a plotting region
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# This function can only add points in one specified cell. Pretending a low-level plotting 
# function, it can only be applied in plotting region which has been created.
#
# You can think the function as the normal `graphics::points`
# function, just adding points in the plotting region. The position of
# plotting region is identified by ``sector.index`` and ``track.index``, if they are not
# specified, they are in 'current' sector and 'current' track.
#
# Data points out of the plotting region will also be added, but with warning messages.
#
# Other graphics parameters which are available in the function are ``pch``, ``col``
# and ``cex`` which have same meaning as those in the `graphics::par`.
#
# Arguments in "..." processed by circlize: 
# Arguments in "..." processed by Rcssplot: pch, col, cex
#
circos.Rcsspoints = function(x, y, sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"), Rcss="default", Rcssclass=c(), ...) {
  
  if(!has.cell(sector.index, track.index)) {
    stop("'circos.points' can only be used after the plotting region has been created\n")
  }
  
  if(length(x) != length(y)) {
    stop("Length of x and y differ.\n")
  }
  
  ## whether the points that are out of the plotting region.
  ## If there is, throw warnings.
  check.points.position(x, y, sector.index, track.index)
  d = circlize(x, y, sector.index, track.index)
  d2 = polar2Cartesian(d)
  Rcsspoints(d2[,1], d2[,2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
  return(invisible(NULL))
}



# == title 
# Add points to the plotting regions in a same track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -track.index  Index for the track
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# The function adds points in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then adding points in each cell by calling `circos.points`.
#
# Length of ``pch``, ``col`` and ``cex`` can be one, length of levels of the factors or length of 
# factors.
#
# This function can be replaced by a ``for`` loop containing `circos.points`.
#
# Arguments in "..." processed by factors: pch, col, cex
# Arguments in "..." processed by circlize:
# Arguments in "..." processed by Rcssplot: 
#
circos.RcsstrackPoints = function(factors = NULL, x, y, track.index = get.cell.meta.data("track.index"),
  Rcss="default", Rcssclass=c(), ...) {
  
  ## basic check here
  if(length(x) != length(factors) || length(y) != length(factors)) {
    stop("Length of data and length of factors differ.\n")
  }

  if(!is.factor(factors)) {
    factors = factor(factors)
  }  
  le = levels(factors)

  ## check whether there are some categories that are not in the circle
  setdiff.factors = setdiff(levels(factors), get.all.sector.index())
  if(length(setdiff.factors)) {
    stop(paste("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".\n", sep = ""))
  }
  
  ## perhaps use col, pch, cex for track points to update Rcss
  Rcss2 = Rcss;
  args = list(...)
  graphics.args = c("col", "pch", "cex", "lwd")
  Rcss2 = updateRcss(Rcss2, le, args, graphics.args, "points", Rcssclass)
  args = args[!(names(args) %in% graphics.args)]    
  ## end of Rcss object update
  
  ## set these graphic parameters with same length as the factors  
  for(i in seq_along(le)) {
    l = factors == le[i]    
    nx = x[l]
    ny = y[l]
    do.call(circos.Rcsspoints,
            c(list(x=nx, y=ny, sector.index = le[i], track.index = track.index,
                   Rcss=Rcss2, Rcssclass=c(Rcssclass, le[i])), args))    
  }
  return(invisible(NULL))
}



# == title 
# Add lines to the plotting region
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -straight     whether draw straight lines between points
# -area         whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls color of the line.
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)

# ==details
# Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circos layout.
# But if you do not want to do such transformation you can use this function just drawing straight
# lines between points by setting ``straight`` to ``TRUE``.
#
# Draw areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be done by specifying
# ``area`` to ``TURE``.
#
# Arguments in "..." processed by circlize: type, baseline
# Arguments in "..." processed by Rcssplot: border, col, lwd, lty, pt.col, cex, pch
#
circos.Rcsslines = function(x, y, sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"),
  straight = FALSE, area = FALSE, 
  Rcss="default", Rcssclass=c(), ...) {
  
  ## replace function arguments by Rcss lookup
  args = list(...)
  if (!hasArg(type)) {
    type = RcssGetPropertyValueOrDefault(Rcss, "circlizelines", "type", default="l", Rcssclass=Rcssclass)
  } else {
    type = args[["type"]]
  }
  if (!hasArg(baseline)) {
    baseline = RcssGetPropertyValueOrDefault(Rcss, "circlizelines", "baseline", default="bottom", Rcssclass=Rcssclass)
  }  else {
    baseline = args[["baseline"]]
  }
  args = args[!(names(args) %in% c("type","baseline"))]
  ## end of Rcss lookup

  if(length(x) != length(y)) {
    stop("Length of x and y differ.\n")
  }
  
  if(baseline == "bottom") {
    baseline = get.cell.meta.data("ylim", sector.index, track.index)[1]
  } else if(baseline == "top") {
    baseline = get.cell.meta.data("ylim", sector.index, track.index)[2]
  }
  
  if(type == "l") {
    
  } else if(type == "o") {
    do.call(circos.Rcsspoints,
            c(list(x=x, y=y, sector.index = sector.index, track.index = track.index,
                   Rcss=Rcss, Rcssclass=Rcssclass), args))
    do.call(circos.Rcsslines,
            c(list(x=x, y=y, sector.index = sector.index, track.index = track.index,
                   straight=FALSE, area=area, Rcss=Rcss, Rcssclass=Rcssclass), args))
    return(invisible(NULL))
  } else if(type == "h") {
    for(i in seq_along(x)) {
      do.call(circos.Rcsslines,
              c(list(x=c(x[i], x[i]), y=c(baseline, y[i]),
                      sector.index = sector.index, track.index = track.index,
                      Rcss=Rcss, Rcssclass=Rcssclass, straight=TRUE, area=FALSE), args))
    }
    return(invisible(NULL))
  } else if(type == "s") {
    d = matrix(nrow = 0, ncol = 2)
    for(i in seq_along(x)) {
      if(i == 1) {
        next
      }
      d = rbind(d, lines.expand(c(x[i-1], x[i]), c(y[i-1], y[i-1]), sector.index, track.index))
      d = rbind(d, cbind(c(x[i], x[i]), c(y[i-1], y[i])))
    }
    
    if(area) {
      ylim = get.cell.meta.data("ylim", sector.index, track.index)
      d = rbind(d, c(d[nrow(d), 1], baseline))
      d = rbind(d, c(d[1, 1], baseline))
      do.call(circos.Rcsspolygon,
              c(list(x=d[, 1], y=d[, 2], sector.index = sector.index, track.index = track.index,
                     Rcss=Rcss, Rcssclass=Rcssclass), args));
    } else {
      do.call(circos.Rcsslines,
              c(list(x=d[, 1], y=d[, 2], sector.index = sector.index, track.index = track.index,
                     Rcss=Rcss, Rcssclass=Rcssclass, straight=TRUE, area=FALSE), args))
    }
    return(invisible(NULL))
  }

  if(!has.cell(sector.index, track.index)) {
    stop("'circos.lines' can only be used after the plotting region been created\n")
  }
  
  ## whether the points that are out of the plotting region.
  check.points.position(x, y, sector.index, track.index)

  if(straight) {
    d = cbind(x, y)
  } else {
    d = lines.expand(x, y, sector.index, track.index)
  }

  if(area) {
    ylim = get.cell.meta.data("ylim", sector.index, track.index)
    d = rbind(d, c(d[nrow(d), 1], baseline))
    d = rbind(d, c(d[1, 1], baseline))
    do.call(circos.Rcsspolygon,
            c(list(x=d[, 1], y=d[, 2], sector.index = sector.index, track.index = track.index,
                   Rcss=Rcss, Rcssclass=Rcssclass), args))
    return(invisible(NULL))
  }
  
  d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
  
  d2polar = polar2Cartesian(d2)
  do.call(Rcsslines, c(list(x=d2polar[,1], y=d2polar[,2], type="l", Rcss=Rcss, Rcssclass=Rcssclass), args))
  return(invisible(NULL))
}



# == title 
# Add lines to the plotting regions in a same track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -track.index  Index for the track
# -straight     whether draw straight lines between points
# -area         whether to fill the area below the lines. If it is set to ``TRUE``, ``col`` controls the filled color
#               in the area and ``border`` controls the color of the line.
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
# The function adds lines in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add lines in cells by calling `circos.lines`.
#
# This function can be replaced by a ``for`` loop containing `circos.lines`.
#
# Arguments in "..." processed by circlize: type, baseline, 
# Arguments in "..." processed by Rcssplot: col, lwd, lty, border, pt.col, cex, pch
#
circos.RcsstrackLines = function(factors, x, y, track.index = get.cell.meta.data("track.index"),
  straight = FALSE, area=FALSE, baseline="bottom", Rcss="default", Rcssclass=c(), ...){
  
  ## basic check here
  if(length(x) != length(factors) || length(y) != length(factors)) {
    stop("Length of data and length of factors differ.\n")
  }
  
  if(!is.factor(factors)) {
    factors = factor(factors)
  }
  
  ## check whether there are some categories that are not in the circle
  setdiff.factors = setdiff(levels(factors), get.all.sector.index())
  if(length(setdiff.factors)) {
    stop(paste("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".\n", sep = ""))
  }
  
  le = levels(factors)
  
  ## set these graphic parameters with same length as the factors
  area = recycle.with.levels(area, le)
  baseline = recycle.with.levels(baseline, le)
  
  ## perhaps use col, pch, cex, lwd for track lines to update Rcss
  Rcss2 = Rcss;  
  args = list(...)
  graphics.args = c("col", "pch", "cex", "lwd", "lty", "pt.col")
  Rcss2 = updateRcss(Rcss2, le, args, graphics.args, "lines", Rcssclass)
  args = args[!(names(args) %in% graphics.args)]
  ## end of Rcss object update
  
  for(i in seq_along(le)) {
    l = factors == le[i]
    nx = x[l]
    ny = y[l]
    do.call(circos.Rcsslines,
            c(list(x=nx, y=ny, sector.index = le[i],
                   track.index = track.index, 
                   straight = straight, area = area[i], baseline=baseline[i],
                   Rcss=Rcss2, Rcssclass=c(Rcssclass, le[i])), args))
  }
  return(invisible(NULL))
}



# == title
# Draw rectangle-like grid
#
# == param
# -xleft        x for the left bottom points
# -ybottom      y for the left bottom points
# -xright       x for the right top points
# -ytop         y for the right top points
# -sector.index Index for the sector
# -track.index  Index for the track
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
# The name for this function is `circos.rect`
# because if you imagine the plotting region as Cartesian coordinate, then it is rectangle.
# in the polar coordinate, the up and bottom edge become two arcs.
#
# You just need to specify the coordinates of two diagonal points just similar as 
# `graphics::rect` does.
#
# Arguments in "..." processed by circlize: [none]
# Arguments in "..." processed by Rcssplot: [same as graphics::rect]
#
circos.Rcssrect = function(xleft, ybottom, xright, ytop,
  sector.index = get.cell.meta.data("sector.index"), 
  track.index = get.cell.meta.data("track.index"), Rcss="default", Rcssclass=c(), ...) {
  
  if(!has.cell(sector.index, track.index)) {
    stop("'circos.Rcssrect' can only be used after the plotting region been created\n")
  }
  
  if(! (length(xleft) == length(ybottom) && length(ybottom) == length(xright) && length(xright) == length(ytop)) ) {
    stop("xleft, ybottom, xright, ytop should have same length.")
  }
  
  x = unlist(lapply(seq_along(xleft), function(i) c(xleft[i], xleft[i], xright[i], xright[i], xleft[i], NA)))
  y = unlist(lapply(seq_along(ybottom), function(i) c(ybottom[i], ytop[i], ytop[i], ybottom[i], ybottom[i], NA)))
  x = x[-length(x)]
  y = y[-length(y)]
  circos.Rcsspolygon(x, y, sector.index = sector.index, track.index = track.index, Rcss=Rcss, Rcssclass=Rcssclass, ...)
  
  return(invisible(NULL))
}



# == title
# Draw polygon
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -sector.index Index for the sector
# -track.index  Index for the track
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
# similar as `graphics::polygon`.
#
# Note: start point should overlap with the end point.
#
# Arguments in "..." processed by circlize: [none]
# Arguments in "..." processed by Rcssplot: [same as graphics::polygon]
#
circos.Rcsspolygon = function(x, y, sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"), Rcss="default", Rcssclass=c(), ...) {
  
  if(!has.cell(sector.index, track.index)) {
    stop("'circos.Rcsspolygon' can only be used after the plotting region been created\n")
  }
  
  ## whether the points that are out of the plotting region.
  check.points.position(x, y, sector.index, track.index)
  
  d = lines.expand(x, y, sector.index, track.index)
  d2 = circlize(d[, 1], d[, 2], sector.index, track.index)
  Rcsspolygon(polar2Cartesian(d2), Rcss=Rcss, Rcssclass=Rcssclass, ...)
  return(invisible(NULL))
}



# == title
# Draw segments through pairwise of points
#
# == param
# -x0 x coordinates for starting points
# -y0 y coordinates for ending points 
# -x1 x coordinates for starting points
# -y1 y coordinates for ending points
# -sector.index Index for the sector
# -track.index  Index for the track
# -straight     whether the segment is a straight line
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
#
# Arguments in "..." processed by circlize: [none]
# Arguments in "..." processed by Rcssplot: [same as graphics::lines]
#
circos.Rcsssegments = function(x0, y0, x1, y1, sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index"), straight = FALSE, Rcss="default", Rcssclass=c(), ...) {
  
  if(!has.cell(sector.index, track.index)) {
    stop("'circos.Rcsssegments' can only be used after the plotting region been created\n")
  }
  
  if(! (length(x0) == length(y0) && length(y0) == length(x1) && length(x1) == length(y1)) ) {
    stop("x0, y0, x1, y1 should have same length.")
  }  
  
  if (!hasArg(type)) {
    type = "l"
  } else {
    type = list(...)[["type"]]
  }
  
  if(length(straight) == 1) straight = rep(straight, length(x0))
  x = NULL
  y = NULL
  for(i in seq_along(x0)) {
    if(straight[i]) {
      x = c(x, c(x0[i], x1[i], NA))
      y = c(y, c(y0[i], y1[i], NA))
    } else {
      d = lines.expand(c(x0[i], x1[i]), c(y0[i], y1[i]), sector.index, track.index)
      x = c(x, c(d[, 1], NA))
      y = c(y, c(d[, 2], NA))
    }
  }
  x = x[-length(x)]
  y = y[-length(y)]
  d2 = circlize(x, y, sector.index, track.index)
  d3 = polar2Cartesian(d2)
  Rcsslines(d3[,1], d3[,2], type=type, Rcss=Rcss, Rcssclass=Rcssclass, ...)
}



# == title
# Draw text in a cell
#
# == param
# -x            Data points on x-axis
# -y            Data points on y-axis
# -labels       Labels for each points
# -sector.index Index for the sector
# -track.index  Index for the track
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
# The function is similar to `graphics::text`. All you need to note is the ``facing`` settings.
#
# Arguments in "..." processed by circlize: facing, niceFacing, adj
# Arguments in "..." processed by Rcssplot: cex, col, font
#
circos.Rcsstext = function(x, y, labels, sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"),  
  Rcss="default", Rcssclass=c(), ...) {  
  
  ## replace arguments by lookup in Rcss object
  args = list(...)  
  if (!hasArg(facing)) {
    facing = RcssGetPropertyValueOrDefault(Rcss, "circlizetext", "facing", default="inside", Rcssclass=Rcssclass)
  } else {
    facing = args[["facing"]]
  }
  if (!hasArg(niceFacing)) {
    niceFacing = RcssGetPropertyValueOrDefault(Rcss, "circlizetext", "niceFacing", default=FALSE, Rcssclass=Rcssclass)
  } else {
    niceFacing = args[["niceFacing"]]
  }
  if (!hasArg(adj)) {
    adj = RcssGetPropertyValueOrDefault(Rcss, "circlizetext", "adj", default=0.5, Rcssclass=Rcssclass)
  } else {
    adj = args[["adj"]]
  }
  args = args[!(names(args) %in% c("facing", "niceFacing", "adj"))]
  ## end of Rcss lookup
  
  if(length(x) != length(y)) {
    stop("Length of x and y differ.\n")
  }
  
  if(!has.cell(sector.index, track.index)) {
    stop("'circos.Rcsstext' can only be used after the plotting region been created\n")
  }

  labels = as.vector(labels)
  
  ## whether the points that are out of the plotting region.
  check.points.position(x, y, sector.index, track.index)
  
  d = circlize(x, y, sector.index, track.index)
  
  ## check facing  
  if(facing == "bending") {
    facing = "bending.inside"
  }
  if(niceFacing && facing %in% c("clockwise", "reverse.clockwise", "inside", "outside", "bending.inside", "bending.outside")) {
    if(facing %in% c("clockwise", "reverse.clockwise")) {
      degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
      degree = degree %% 360
      l1 = degree >= 90 & degree < 270  # should be reverse.clockwise
      l2 = !l1  # should be clockwise
      if(facing == "reverse.clockwise") {
        adj1 = adj
        adj2 = 1 - adj
        facing1 = "reverse.clockwise"
        facing2 = "clockwise"
      } else {
        adj1 = 1- adj
        adj2 = adj
        facing1 = "reverse.clockwise"
        facing2 = "clockwise"
      }
    } else if(facing %in% c("inside", "outside", "bending.inside", "bending.outside")) {
      degree = circlize(x, y, sector.index = sector.index, track.index = track.index)[, 1]
      degree = degree %% 360
      l1 = degree > 0 & degree < 180  # should be inside
      l2 = !l1   # should be outside
      if(facing == "inside") {
        adj1 = adj
        adj2 = 1 - adj
        facing1 = "inside"
        facing2 = "outside"
      } else if(facing == "outside") {
        adj1 = 1 - adj
        adj2 = adj
        facing1 = "inside"
        facing2 = "outside"
      } else if(facing == "bending.inside") {
        adj1 = adj
        adj2 = 1 - adj
        facing1 = "bending.inside"
        facing2 = "bending.outside"
      } else if(facing == "bending.outside") {
        adj1 = 1 - adj
        adj2 = adj
        facing1 = "bending.inside"
        facing2 = "bending.outside"
      }
    }
    if(sum(l1)) {
      do.call(circos.Rcsstext,
              c(list(x=x[l1], y=y[l1], labels=labels[l1], sector.index = sector.index,
                     track.index = track.index, facing = facing1, niceFacing = FALSE, adj = adj1, Rcss=Rcss, Rcssclass=Rcssclass), args))
    }
    
    if(sum(l2)) {
      do.call(circos.Rcsstext,
              c(list(x=x[l2], y=y[l2], labels=labels[l2], sector.index = sector.index,
                     track.index = track.index, facing = facing2, niceFacing = FALSE, adj = adj2, Rcss=Rcss, Rcssclass=Rcssclass), args))
    }
    return(invisible(NULL))
  } 

  if(grepl("bending", facing)) {
    
    chars = strsplit(labels, "")
    if(facing == "bending.outside") {
      chars = lapply(chars, rev)
    }
    
    nlabel = length(labels)
    strw = lapply(chars, strwidth)
    strh = lapply(chars, strheight)
    
    if(facing == "bending.outside") {
      adj = 1 - adj
    }
    
    alpha.offset = sapply(strw, function(x) sum(x))*adj[1]/d[, 2] * 180/pi
    rou.offset = sapply(strh, function(x) -x[1]*adj[2])
    
    for(i in seq_along(labels)) {
      ## degree of the bottom center of each char
      theta = numeric(length(strw[[i]]))
      alpha = d[i, 1] + alpha.offset[i]
      rou = d[i, 2] + rou.offset[i]
      
      for(j in  seq_along(strw[[i]])) {
        theta[j] = alpha - asin(strw[[i]][j]/2/d[i, 2])*180/pi
        alpha = alpha - asin(strw[[i]][j]/d[i, 2])*180/pi
      }
      dr = reverse.circlize(theta, rep(rou, length(theta)), sector.index, track.index)
      
      if(facing == "bending.inside") {
        do.call(circos.Rcsstext,
                c(list(x=dr[, 1], y=dr[, 2], labels = chars[[i]], sector.index = sector.index, track.index = track.index, facing = "inside", adj = c(0.5, 0),
                       Rcss=Rcss, Rcssclass=Rcssclass), args))
      } else if(facing == "bending.outside") {
        do.call(circos.Rcsstext,
                c(list(x=dr[, 1], y=dr[, 2], labels = chars[[i]], sector.index = sector.index, track.index = track.index, facing = "outside", adj = c(0.5, 1),
                       Rcss=Rcss, Rcssclass=Rcssclass), args))
      }
    }
    
  } else {
    
    srt = d[,1]-90    #srt = ifelse(srt > 0, srt, 360 + srt)
    
    if(facing == "reverse.clockwise") {           # pointing to the circle center, but facing left at 90 degree
      srt = srt - 90
    } else if(facing == "clockwise") {   # pointing to the circle center, but facing right at 90 degree
      srt = srt + 90
    } else if(facing == "downward") {       # horizontal at the finnal graph
      srt = rep(0, length(srt))
    } else if(facing == "outside") {
      srt = srt + 180
    }
    
    m = polar2Cartesian(d)
    for(i in seq_along(x)) {
      do.call(Rcsstext,
              c(list(x=m[i, 1], y=m[i, 2], labels = labels[i], srt = srt[i], adj = adj, Rcss=Rcss, Rcssclass=Rcssclass), args))
    }
  }

  return(invisible(NULL))
}



# == title
# Draw text in cells among the whole track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data points on x-axis
# -y            Data points on y-axis
# -labels       Labels
# -track.index  Index for the track
# -Rcss         Rcss style object
# -Rcssclass    sub class for style sheet
# -...          Further graphical parameters (see details)
#
# == details
# The function adds texts in multiple cells by first splitting data into several parts in which
# each part corresponds to one factor (sector index) and then add texts in cells by calling `circos.text`.
#
# This function can be replaced by a ``for`` loop containing `circos.text`.
#
# Arguments in "..." processed by factors: cex, col, font
# Arguments in "..." processed by circlize: facing, niceFacing, adj 
# Arguments in "..." processed by Rcssplot: 
#
circos.RcsstrackText = function(factors, x, y, labels, track.index = get.cell.meta.data("track.index"),
  Rcss="default", Rcssclass=c(), ...) {

  ## basic check here
  if(length(x) != length(factors) || length(y) != length(factors)) {
    stop("Length of data and length of factors differ.\n")
  }
  
  if(!is.factor(factors)) {
    factors = factor(factors)
  }
  
  ## check whether there are some categories that are not in the circle
  setdiff.factors = setdiff(levels(factors), get.all.sector.index())
  if(length(setdiff.factors)) {
    stop(paste("Cannot find these categories in existed sectors:", paste(setdiff.factors, collapse = ", "), ".\n", sep = ""))
  }
  
  le = levels(factors)
  
  ## use graphical settings to update Rcss object
  Rcss2 = Rcss;
  args = list(...)
  graphics.args = c("cex", "col", "font")
  Rcss2 = updateRcss(Rcss2, le, args, graphics.args, "text", Rcssclass)  
  args = args[!(names(args) %in% graphics.args)]
  ## end of update of Rcss object
  
  for(i in seq_along(le)) {
    l = factors == le[i]
    nx = x[l]
    ny = y[l]
    nlabels = labels[l]
    circos.Rcsstext(nx, ny, sector.index = le[i], track.index = track.index, labels = nlabels,                   
                    Rcss=Rcss2, Rcssclass=c(Rcssclass, le[i]), args)    
  }
  return(invisible(NULL))
}



# == title
# Draw x-axis
#
# == param
# -h                Position of the x-axis, can be "top", "bottom" or a numeric value
# -major.at         If it is numeric vector, it identifies the positions
#                   of the major ticks. It can exceed ``xlim`` value and the exceeding part
#                   would be trimmed automatically. If it is ``NULL``, about every 10 degrees there is a major tick.
# -labels           labels of the major ticks. Also, the exceeding part would be trimmed automatically.
# -major.tick       Whether to draw major tick. If it is set to ``FALSE``, there would be
#                   no minor ticks.
# -sector.index     Index for the sector
# -track.index      Index for the track
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
#
# == details
# It can only draw axes on x-direction.
#
# Arguments in "..." processed by circlize: labels.facing, labels.niceFacing, minor.ticks,
#                                           major.tick.percentage, major.tick, minor.ticks,
#                                           direction, labels.away.percentage
# Arguments in "..." processed by Rcssplot: labels.font, labels.cex, lwd
#
circos.Rcssaxis = function(h = "top", major.at = NULL, labels = TRUE, 
  sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"),
  Rcss="default", Rcssclass=c(), ...) {   

  ## replace function arguments by lookup in Rcssplot object
  args = list(...)
  if (!hasArg(labels.facing)) {
    labels.facing = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "labels.facing", default="inside", Rcssclass=Rcssclass)
  } else {
    labels.facing = args[["labels.facing"]]
  }
  if (!hasArg(minor.ticks)) {
    minor.ticks = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "minor.ticks", default=4, Rcssclass=Rcssclass)
  } else {
    minor.ticks = args[["minor.ticks"]]
  }
  if (!hasArg(labels.niceFacing)) {
    labels.niceFacing = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "labels.niceFacing", default=TRUE, Rcssclass=Rcssclass)
  } else {
    labels.niceFacing = args[["labels.niceFacing"]]
  }
  if (!hasArg(major.tick.percentage)) {
    major.tick.percentage = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "major.tick.percentage", default=0.1, Rcssclass=Rcssclass);
  } else {
    major.tick.percentage = args[["major.tick.percentage"]]
  }
  if (!hasArg(labels.away.percentage)) {
    labels.away.percentage = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis",
      "labels.away.percentage", default=major.tick.percentage/2, Rcssclass=Rcssclass);
  } else {
    labels.away.percentage = args[["labels.away.percentage"]]
  }
  if (!hasArg(major.tick)) {
    major.tick =  RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "major.tick", default=1, Rcssclass=Rcssclass);
  } else {
    major.tick = args[["major.tick"]]
  }
  if (!hasArg(direction)) {
    direction = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "direction", default="outside", Rcssclass=Rcssclass)
  } else {
    direction = args[["direction"]]
  }
  args = args[!(names(args) %in% c("labels.facing", "minor.ticks", "labels.niceFacing",
    "major.tick.percentage", "labels.away.percentage", "major.tick", "direction"))]
  ## end of Rcss lookup

  
  if(! direction %in% c("outside", "inside")) {
    stop("Direction should be in 'outside' and 'inside'.\n")
  }
  
  xlim = get.cell.meta.data("xlim", sector.index, track.index)
  
  sector.data = get.sector.data(sector.index)
  
  if(h == "top") {
    h = get.cell.meta.data("cell.ylim", sector.index, track.index)[2]
  } else if(h == "bottom") {
    h = get.cell.meta.data("cell.ylim", sector.index, track.index)[1]
  }
  
  if(is.null(major.at)) {
    major.by = .default.major.by(sector.index, track.index)
    major.at = seq(floor(xlim[1]/major.by)*major.by, xlim[2], by = major.by)
    major.at = c(major.at, major.at[length(major.at)] + major.by)
  }
  
  minor.at = NULL
  if(minor.ticks != 0) {
    for(i in seq_along(major.at)) {
      if(i == 1) next
      k = seq_len(minor.ticks) / (minor.ticks + 1)
      minor.at = c(minor.at, k * (major.at[i] - major.at[i - 1]) + major.at[i - 1])
    }
  }

  xlim2 = xlim
  circos.Rcsslines(c(ifelse(major.at[1] >= xlim2[1], major.at[1], xlim2[1]),
                     ifelse(major.at[length(major.at)] <= xlim2[2], major.at[length(major.at)], xlim2[2])), 
                   c(h, h), sector.index = sector.index, track.index = track.index,
                   straight=FALSE, area=FALSE,
                   Rcss=Rcss, Rcssclass=Rcssclass, args)
  
  ## ticks
  yrange = get.cell.meta.data("yrange", sector.index, track.index)
  major.tick.length = yrange * major.tick.percentage
  
  op = circos.par("points.overflow.warning")
  circos.par("points.overflow.warning" = FALSE)
  l = major.at >= xlim2[1] & major.at <= xlim2[2]
  if(major.tick) {
    circos.Rcsssegments(major.at[l], rep(h, sum(l)), major.at[l], rep(h, sum(l)) + major.tick.length*ifelse(direction == "outside", 1, -1), straight = TRUE,
                        sector.index = sector.index, track.index = track.index, type="l", Rcss=Rcss, Rcssclass=Rcssclass, args)
  }
  
  labels.adj = NULL
  if(direction == "outside") {
    if(labels.facing == "inside") {
      labels.adj = c(0.5, 0)
    } else if(labels.facing == "outside") {
      labels.adj = c(0.5, 1)
    } else if(labels.facing == "reverse.clockwise") {
      labels.adj = c(1, 0.5)
    } else if(labels.facing == "clockwise") {
      labels.adj = c(0, 0.5)
    } else if(labels.facing == "downward") {
      labels.adj = c(0.5, 0.5)
    } else {
      labels.adj = c(0.5, 0)
    }
  } else {
    if(labels.facing == "inside") {
      labels.adj = c(0.5, 1)
    } else if(labels.facing == "outside") {
      labels.adj = c(0.5, 0)
    } else if(labels.facing == "reverse.clockwise") {
      labels.adj = c(0, 0.5)
    } else if(labels.facing == "clockwise") {
      labels.adj = c(1, 0.5)
    } else if(labels.facing == "downward") {
      labels.adj = c(0.5, 0.5)
    } else {
      labels.adj = c(0.5, 1)
    }
  }
  
  if(is.logical(labels) && labels) {
    circos.Rcsstext(major.at[l], rep(h, sum(l)) + (major.tick.length+yrange*labels.away.percentage)*ifelse(direction == "outside", 1, -1),
                    labels = major.at[l], adj = labels.adj,
                    sector.index = sector.index, track.index = track.index,
                    facing = labels.facing, niceFacing = labels.niceFacing, Rcss=Rcss, Rcssclass=Rcssclass)
  } else if(is.logical(labels) && !labels) {
    
  } else if(length(labels)) {
    circos.Rcsstext(major.at[l], rep(h, sum(l)) + (major.tick.length+yrange*labels.away.percentage)*ifelse(direction == "outside", 1, -1),
                    labels = labels[l], adj = labels.adj,
                    sector.index = sector.index, track.index = track.index,
                    facing = labels.facing, niceFacing = labels.niceFacing, Rcss=Rcss, Rcssclass=Rcssclass)
  }				
  

  if(major.tick) {
    
    l = minor.at >= xlim2[1] & minor.at <= xlim2[2]
    circos.Rcsssegments(minor.at[l], rep(h, sum(l)), minor.at[l], rep(h, sum(l)) + major.tick.length/2*ifelse(direction == "outside", 1, -1), straight = TRUE,
                        sector.index = sector.index, track.index = track.index, type="l", Rcss=Rcss, Rcssclass=Rcssclass, args)
  }
  
  circos.par("points.overflow.warning" = op)
  return(invisible(NULL))
}



# == title
# Draw x-axis
#
# == param
# -... all pass to `circos.Rcssaxis`
#
circos.Rcssxaxis = function(...) {
	circos.Rcssaxis(...)
}



# == title
# Draw y-axis
#
# == param
# -side add the y-axis on the left or right of the cell
# -at         If it is numeric vector, it identifies the positions
#                   of the ticks. It can exceed ``ylim`` value and the exceeding part
#                   would be trimmed automatically.
# -labels           labels of the ticks. Also, the exceeding part would be trimmed automatically.
# -tick       Whether to draw ticks.
# -sector.index     Index for the sector
# -track.index      Index for the track
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# Note, you need to set the gap between sectors manually by `circos.par` to make sure there is enough space
# for y-axis.
#
# Arguments in "..." processed by circlize: labels.niceFacing, tick.length
# Arguments in "..." processed by Rcssplot: lwd, font, cex
#
circos.Rcssyaxis = function(side = c("left", "right"), at = NULL, labels = TRUE, tick = TRUE,
  sector.index = get.cell.meta.data("sector.index"),
  track.index = get.cell.meta.data("track.index"),
  Rcss="default", Rcssclass=c()) {
  
  ylim = get.cell.meta.data("ylim", sector.index, track.index)
  
  side = match.arg(side)[1]
  if(side == "left") {
    v = get.cell.meta.data("cell.xlim", sector.index, track.index)[1]
  } else if(side == "right") {
    v = get.cell.meta.data("cell.xlim", sector.index, track.index)[2]
  }
  
  if(is.null(at)) {
    at = grid.pretty(ylim)
    labels = at
  }
  
  ylim2 = ylim
  
  ## replace arguments by lookup in Rcss object
  args = list(...)  
  if (!hasArg(labels.niceFacing)) {
    labels.niceFacing = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "labels.niceFacing", default=TRUE, Rcssclass=Rcssclass)
  } else {
    labels.niceFacing = args[["labels.niceFacing"]]
  }
  if (!hasArg(tick.length)) {
    tick.length = RcssGetPropertyValueOrDefault(Rcss, "circlizeaxis", "tick.length", default=0.5, Rcssclass=Rcssclass)
  } else {
    tick.length = args[["tick.length"]]
  }  
  ## end of Rcss lookup
  
  ## ticks
  yrange = get.cell.meta.data("yrange", sector.index, track.index)
  xrange = get.cell.meta.data("xrange", sector.index, track.index)
  tick.length = tick.length/abs(get.cell.meta.data("cell.start.degree", sector.index, track.index) - get.cell.meta.data("cell.end.degree", sector.index, track.index)) * xrange
  
  op = circos.par("points.overflow.warning")
  circos.par("points.overflow.warning" = FALSE)
  l = at >= ylim2[1] & at <= ylim2[2]
  if(tick) {
    circos.Rcsssegments(rep(v, sum(l)), at[l], rep(v, sum(l)) + tick.length*ifelse(side == "right", 1, -1), at[l], straight = TRUE,
                    sector.index = sector.index, track.index = track.index, Rcss=Rcss, Rcssclass=c(Rcssclass, "ticks"))
  }
  
  labels.adj = NULL
  if(side == "left") {
    labels.adj = c(1, 0.5)
  } else {
    labels.adj = c(0, 0.5)
  }
  
  if(is.logical(labels) && labels) {
    circos.Rcsstext(rep(v, sum(l)) + (tick.length*1.5)*ifelse(side == "right", 1, -1), at[l], 
                    labels = at[l], adj = labels.adj,
                    sector.index = sector.index, track.index = track.index,
                    facing = "inside", niceFacing = labels.niceFacing, Rcss=Rcss, Rcssclass=c(Rcssclass, "yaxis"))
  } else if(is.logical(labels) && !labels) {
    
  } else if(length(labels)) {
    circos.Rcsstext(rep(v, sum(l)) + (tick.length*1.5)*ifelse(side == "right", 1, -1), at[l],
                    labels = labels[l], adj = labels.adj,
                    sector.index = sector.index, track.index = track.index,
                    facing = "inside", niceFacing = labels.niceFacing, Rcss=Rcss, Rcssclass=c(Rcssclass, "yaxis"))
  }				
  
  circos.par("points.overflow.warning" = op)
  return(invisible(NULL))
}

