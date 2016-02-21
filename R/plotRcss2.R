## ##################################################################
##
## This is part2 or plotRcss (circlize package)
## This part contains functions for high-level graphics (barplot, hist, etc)
##
## ##################################################################


#####################################################################
#
# simulate high-level graphic functions such as barplot, hist, boxplot ...
#
#####################################################################

# == title
# Draw histogram in cells among a whole track
#
# == param
# -factors      Factors which represent the categories of data
# -x            Data on the x-axis
# -track.index  Index for the track which is going to be updated. Setting it to ``NULL`` means
#               creating the plotting regions in the next newest track.
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``. Btw, ``ylim`` is calculated automatically.
# -breaks       see `graphics::hist`
# -include.lowest see `graphics::hist`
# -right          see `graphics::hist`
# -draw.density   whether draw density lines instead of histogram bars.
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
# 
# == details
# It draw histogram in cells among a whole track. It is also an example to show how to add self-defined
# high-level graphics by this package.
#
# Settings from selector "circlize" in Rcss object:
#    track.height
# Settings from selector "circlizeregions" in Rcss object:
##   bg.col, bg.border, bg.lty, bg.lwd
# Arguments in "..." processed by Rcssplot:
#    col, lwd, lty, border
#
circos.RcsstrackHist = function(factors, x, 
  track.index = NULL, force.ylim = TRUE,
  breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE,
  Rcss="default", Rcssclass=c(), ...) {

  ## basic check here
  if(length(x) != length(factors)) {
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

  ## calculate the distributions
  le = levels(factors)
  
  xx = NULL
  yy = NULL
  fa = NULL
  
  for(i in seq_along(le)) {
    l = factors == le[i]
    nx = x[l]
    
    h = hist(nx, plot = FALSE, breaks = breaks, include.lowest = include.lowest, right = right)
    
    xx = c(xx, h$breaks)
    if(draw.density) {
      yy = c(yy, 0, h$density)
    } else {
      yy = c(yy, 0, h$counts)
    }
    
    fa = c(fa, rep(le[i], length(h$breaks)))
  }

  ## extract settings for regions
  args = list(...)
  regions.args = c("track.height", "bg.col", "bg.font", "bg.lty", "bg.lwd")
  args = args[names(args) %in% regions.args]
  
  ## create the plotting region
  do.call(circos.RcsstrackPlotRegion,
          c(list(factors = fa, y=yy,
                 track.index = track.index, force.ylim = force.ylim,  
                 Rcss=Rcss, Rcssclass=Rcssclass), args))
  track.index = get.current.track.index()
  
  l3 = logical(0)
  for(i in seq_along(le)) {
    xlim = get.cell.meta.data("xlim", sector.index = le[i], track.index = track.index)
    l = fa == le[i]
    l2 = xx[l] >= xlim[1] & xx[l] <= xlim[2]
    l3 = c(l3, l2)
  }
  
  xx = xx[l3]
  yy = yy[l3]
  fa = fa[l3]

  ## remove graphical settings for tracks 
  args = list(...)
  args = args[!(names(args) %in% regions.args)]
  ## shift graphical settings for rectangles into the Rcss for polygons
  Rcss2 = Rcss;
  graphics.args = c("col", "border", "lty", "lwd")
  Rcss2 = updateRcss(Rcss2, le, args, graphics.args, "polygon", Rcssclass)
  Rcss2 = updateRcss(Rcss2, le, args, graphics.args, "lines", Rcssclass)  
  args = args[!(names(args) %in% graphics.args)] 
  ## end of update of Rcss object
  
  if(draw.density) {
    do.call(circos.RcsstrackLines,
            c(list(factors = fa, x=xx, y=yy, track.index = track.index,
                   Rcss=Rcss, Rcssclass=Rcssclass), args))
  } else {
    ## in each cell, draw rectangles
    for(i in seq_along(le)) {
      l = fa == le[i]
      
      nx = xx[l]
      ny = yy[l]
      
      cell.xlim = get.cell.meta.data("cell.xlim", le[i], track.index)
      nx[nx < cell.xlim[1]] = cell.xlim[1]    
      nx[nx > cell.xlim[2]] = cell.xlim[2]
      
      for(j in seq_along(nx)) {
        if(j == 1) {
          next
        }
        
        do.call(circos.Rcssrect,
                c(list(xleft=nx[j-1], ybottom=0, xright=nx[j], ytop=ny[j],
                       sector.index = le[i], track.index = track.index,
                       Rcss=Rcss, Rcssclass=c(Rcssclass, le[i])), args))
      }
    }
  }
  return(invisible(NULL))
}



# == title
# Draw sectors or rings in a circle
#
# == param
# -start.degree   start degree for the sector
# -end.degree     end degree for the sector
# -rou1           Radius for one of the arc in the sector
# -rou2           Radius for the other arc in the sector
# -center         Center of the circle
# -clock.wise     The direction from ``start.degree`` to ``end.degree``
# -Rcss           Rcss style object
# -Rcssclass      sub class for style sheet
# -...            Further graphical parameters (see details)
#
# == details
# If the interval between ``start`` and ``end`` (larger or equal to 360 or smaller or equal to -360)
# it would draw a full circle or ring. If ``rou2`` is set, it would draw part of a ring.
#
# Settings from selector "circlize" in Rcss object:
#  
# Settings from selector "circlizeregions" in Rcss object:
## 
# Arguments in "..." processed by Rcssplot:
#    col, lwd, lty, border

draw.Rcsssector = function(start.degree = 0, end.degree = 360, rou1 = 1, rou2 = NULL,
  center = c(0, 0), clock.wise = TRUE, 
  Rcss="default", Rcssclass=c(), ...) {

  warning("Rcss features not implemented yet")
  
  is.circular = function(start.degree, end.degree) {
    (end.degree - start.degree) %% 360 == 0 && (end.degree - start.degree) != 0
  }
  
  degree_diff = function(start, end, clock.wise = TRUE) {
    if(is.circular(start, end)) {
      360
    } else {
      start = start %% 360
      end = end %% 360
      if(clock.wise) (start - end) %% 360
      else (end - start) %% 360
    }
  }
  
  ## from start to end
  degree_seq = function(start, end, clock.wise = TRUE, ...) {
    if(is.circular(start, end)) {
      seq(0, 360, ...)
    } else {
      start = start %% 360
      end = end %% 360
      if(clock.wise) {
        ## make start is larger than end, but the difference is less than 360
        if(start < end) start = start + 360
        seq(start, end, ...)
      } else {
        if(start > end) start = start - 360
        seq(start, end, ...)
      }
    }
  }
  
  d1 = NULL
  
  ## calculate the number of segments of the up arc
  l1 = as.radian(degree_diff(start.degree, end.degree, clock.wise)) * rou1
  ncut1 = l1/ (2*pi/circos.par("unit.circle.segments"))
  ncut1 = floor(ncut1)
  ncut1 = ifelse(ncut1 < 2, 2, ncut1)
  
  ## d1 is from the start.degree to end.degree
  d1 = rbind(d1, cbind(degree_seq(start.degree, end.degree, clock.wise, length.out = ncut1), rep(rou1, ncut1)))
  
  ## d2 is from end.degree to start.degree
  d2 = NULL
  if(!is.null(rou2)) {
    ## calculate the number of segments of the bottom arc
    l2 = as.radian(degree_diff(start.degree, end.degree, clock.wise)) * rou2
    ncut2 = l2/ (2*pi/circos.par("unit.circle.segments"))
    ncut2 = floor(ncut2)
    ncut2 = ifelse(ncut2 < 2, 2, ncut2)
    
    d2 = rbind(d2, cbind(degree_seq(end.degree, start.degree, !clock.wise, length.out = ncut2), rep(rou2, ncut2)))
  }
  
  if(is.null(rou2)) {
    m1 = polar2Cartesian(d1)
    if(is.circular(start.degree, end.degree)) {  # it is a circle
      m = m1
    } else {
      m = rbind(m1, c(0, 0))
    }
    
    ## and shift to the center
    m[, 1] = m[, 1] + center[1]
    m[, 2] = m[, 2] + center[2]
    Rcsspolygon(m, Rcss=Rcss, Rcssclass=Rcssclass, ...)
  } else {
    m1 = polar2Cartesian(d1)
    m2 = polar2Cartesian(d2)
    
    if(is.circular(start.degree, end.degree)) {  # a ring
      m = rbind(m1, m2[rev(seq_len(nrow(m2))), ,drop = FALSE])
      
      m[, 1] = m[, 1] + center[1]
      m[, 2] = m[, 2] + center[2]
      Rcsspolygon(m[,1], m[,2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
      ## two borders
      Rcsslines(m1[, 1]+center[1], m1[, 2]+center[2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
      Rcsslines(m2[, 1]+center[1], m2[, 2]+center[2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
      
    } else {
      m = rbind(m1, m2)
      
      m[, 1] = m[, 1] + center[1]
      m[, 2] = m[, 2] + center[2]
      Rcsspolygon(m[,1], m[,2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
    }
    
  } 
  
  return(invisible(NULL))
}


# == title
# Highlight sectors and tracks
#
# == param
# -sector.index A vector of sector index
# -track.index A vector of track index that you want to highlight
# -col Color for highlighting. Note the color should be semi-transparent.
# -border Border of the highlighted region
# -lwd Width of borders
# -lty Style of borders
# -padding Padding for the highlighted region. It should contain four values
#          representing ratios of the width or height of the highlighted region
# -text text added in the highlight region, only support plotting one string at a time
# -text.vjust adjustment on 'vertical' (radical) direction
# -text.col color for the text
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#                   (old: pass to `circos.text`)
#
# == details
# You can use `circos.info` to find out index for all sectors and all tracks.
#
# The function calls `draw.sector`.
highlight.Rcsssector = function(sector.index, track.index = get.all.track.index(), 
  col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
  padding = c(0, 0, 0, 0), text = NULL, text.col = par("col"), 
  text.vjust = 0.5,
  Rcss="default", Rcssclass=c(), ...) {

  warning("Rcss features not implemented yet")
  
  sectors = get.all.sector.index()
  if(!all(sector.index %in% sectors)) {
    stop("`chr` contains index that does not beling to available sectors")
  }
  tracks = get.all.track.index()
  if(!all(track.index %in% tracks)) {
    stop("`track.index` contains index that does not belong to available tracks.\n")
  }
  
  ## if all sectors are selected
  if(length(setdiff(sectors, sector.index)) == 0) {
    track.index = sort(unique(track.index))
    ts = continuousIndexSegment(track.index)
    
    for(i in seq_along(ts)) {
      track.index.vector = ts[[i]]
      start.degree = 0
      end.degree = 360
      rou1 = get.cell.meta.data("cell.top.radius", sectors[1], track.index.vector[1])
      rou2 = get.cell.meta.data("cell.bottom.radius", sectors[1], track.index.vector[length(track.index.vector)])
      
      d2 = rou1 - rou2
      rou1 = rou1 + d2*padding[3]
      rou2 = rou2 - d2*padding[1]
      
      draw.sector(start.degree = start.degree, end.degree = end.degree, rou1 = rou1, rou2 = rou2, col = col, border = border, lwd = lwd, lty = lty)
      
      if(!is.null(text)) {
        ## map to most recent cell
        pos = reverse.circlize((start.degree + end.degree)/2, (rou1 + rou2)/2)
        op_warning = circos.par("points.overflow.warning")
        circos.text(pos[1,1], pos[1,2], text, adj = c(0.5, text.vjust), col = text.col, ...)
        circos.par(points.overflow.warning = op_warning)
      }
    }
    
  } else {
    
    sector.numeric.index = which(sectors %in% sector.index)
    ss = continuousIndexSegment(sector.numeric.index, n = length(sectors), loop = TRUE)
    
    track.index = sort(unique(track.index))
    ts = continuousIndexSegment(track.index)
    
    for(j in seq_along(ss)) {
      sector.index.vector = sectors[ ss[[j]] ]
      for(i in seq_along(ts)) {
        track.index.vector = ts[[i]]
        start.degree = get.cell.meta.data("cell.start.degree", sector.index.vector[1], track.index = 1)
        end.degree = get.cell.meta.data("cell.end.degree", sector.index.vector[length(sector.index.vector)], track.index = 1)
        rou1 = get.cell.meta.data("cell.top.radius", sector.index.vector[1], track.index.vector[1])
        rou2 = get.cell.meta.data("cell.bottom.radius", sector.index.vector[1], track.index.vector[length(track.index.vector)])
        
        d1 = end.degree - start.degree
        d2 = rou1 - rou2
        start.degree = start.degree - d1*padding[2]
        end.degree = end.degree + d1*padding[4]
        rou1 = rou1 + d2*padding[3]
        rou2 = rou2 - d2*padding[1]
        
        draw.sector(start.degree = start.degree, end.degree = end.degree, rou1 = rou1, rou2 = rou2, col = col, border = border, lwd = lwd, lty = lty)
        if(!is.null(text)) {
          ## map to most recent cell
          pos = reverse.circlize((start.degree + end.degree)/2, (rou1 + rou2)/2)
          op_warning = circos.par("points.overflow.warning")
          circos.text(pos[1,1], pos[1,2], text, adj = c(0.5, text.vjust), col = text.col, ...)
          circos.par(points.overflow.warning = op_warning)
        }
      }
    }
  }	
}


# == title
# Add circlized dendrograms
#
# == param
# -dend A `stats::dendrogram` object.
# -facing Is the dendromgrams facing inside to the circle or outside.
# -max_height Maximum height of the dendrogram. This is important if more than one dendrograms
#             are drawn in one track and making them comparable.
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# Assuming there are ``n`` nodes in the dendrogram, the positions for leaves on x-axis is ``0.5, 1.5, ..., n - 0.5``.
# So you must be careful with ``xlim`` when you initialize the cirular layout.
#
# You can use the ``dendextend`` package to render the dendrograms. (Is this still true with Rcss?)
#
# Using Rcss objects:
# Style lines using subclasses "edge1" and "edge2"
# Style points using subclasses "branch", "leaf1", "leaf2"
# 
circos.Rcssdendrogram = function(dend, facing = c("outside", "inside"), max_height = NULL,
 Rcss="default", Rcssclass=c(), ... ) {

  facing = match.arg(facing)[1]
  
  if(is.null(max_height)) {
    max_height = attr(dend, "height")
  }
  
  is.leaf = function(object) {
    leaf = attr(object, "leaf")
    if(is.null(leaf)) {
      FALSE
    } else {
      leaf
    }
  }
  
  draw.d = function(dend, max_height, facing = "outside", max_width = 0) {
    leaf = attr(dend, "leaf")
    d1 = dend[[1]]  # child tree 1
    d2 = dend[[2]]  # child tree 2
    height = attr(dend, "height")
    midpoint = attr(dend, "midpoint")
    
    if(is.leaf(d1)) {
      x1 = x[as.character(attr(d1, "label"))]
    } else {
      x1 = attr(d1, "midpoint") + x[as.character(labels(d1))[1]]
    }
    y1 = attr(d1, "height")
    
    if(is.leaf(d2)) {
      x2 = x[as.character(attr(d2, "label"))]
    } else {
      x2 = attr(d2, "midpoint") + x[as.character(labels(d2))[1]]
    }
    y2 = attr(d2, "height")
    
    ## plot the connection line and branch points    
    circos.Rcsslines(c(x1, x1), max_height - c(y1, height), straight = TRUE, Rcss=Rcss, Rcssclass=c(Rcssclass, "eddge1"), ...)
    circos.Rcsslines(c(x1, (x1+x2)/2), max_height - c(height, height), straight=FALSE, Rcss=Rcss, Rcssclass=c(Rcssclass, "edge1"), ...)
    circos.Rcsslines(c(x2, x2), max_height - c(y2, height), straight = TRUE, Rcss=Rcss, Rcssclass=c(Rcssclass, "edge2"), ...)
    circos.Rcsslines(c(x2, (x1+x2)/2), max_height - c(height, height), straigh=FALSE, Rcss=Rcss, Rcssclass=c(Rcssclass, "edge2"), ...)      
    circos.Rcsspoints((x1+x2)/2, max_height - height, Rcss=Rcss, Rcssclass=c(Rcssclass, "branch"), ...)      
    
    ## do it recursively
    if(is.leaf(d1)) {
      circos.Rcsspoints(x1, max_height, Rcss=Rcss, Rcssclass=c(Rcssclass,"leaf1"), ...) 
    } else {
      draw.d(d1, max_height, facing, max_width)
    }
    
    if(is.leaf(d2)) {
      circos.Rcsspoints(x2, max_height, Rcss=Rcss, Rcssclass=c(Rcssclass, "leaf2"), ...) 
    } else {
      draw.d(d2, max_height, facing, max_width)
    }
  }
  
  labels = as.character(labels(dend))
  x = seq_along(labels) - 0.5
  
  names(x) = labels
  n = length(labels)
  
  draw.d(dend, max_height, facing, max_width = n)
}

