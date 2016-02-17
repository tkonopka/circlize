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
# -track.height Height of the track. It is the percentage to the radius of the unit circle.
#               If to update a track, this argument is disabled.
# -force.ylim   Whether to force all cells in the track to share the same ``ylim``. Btw, ``ylim`` is calculated automatically.
# -col          Filled color for histogram
# -border       Border color for histogram
# -lty          Line style for histogram
# -lwd          Line width for histogram
# -bg.col       Background color for the plotting regions
# -bg.border    Color for the border of the plotting regions
# -bg.lty       Line style for the border of the plotting regions
# -bg.lwd       Line width for the border of the plotting regions
# -breaks       see `graphics::hist`
# -include.lowest see `graphics::hist`
# -right          see `graphics::hist`
# -draw.density   whether draw density lines instead of histogram bars.
# 
# == details
# It draw histogram in cells among a whole track. It is also an example to show how to add self-defined
# high-level graphics by this package.
circos.RcsstrackHist = function(factors, x, track.height = circos.par("track.height"),
  track.index = NULL, force.ylim = TRUE, col = ifelse(draw.density, "black", NA),
  border = "black", lty = par("lty"), lwd = par("lwd"),
  bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
  breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE) {

  warning("Rcss features not implemented yet")
  
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
  
  ## create the plotting region
  circos.trackPlotRegion(factors = fa, y=yy, track.height = track.height,
                         track.index = track.index, force.ylim = force.ylim,
                         bg.col = bg.col, bg.border = bg.border, bg.lty = bg.lty, bg.lwd = bg.lwd)
  
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
  
  if(draw.density) {
    circos.trackLines(factors = fa, xx, yy, track.index = track.index,
                      col = col, lty = lty, lwd = lwd)
  } else {
    ## in each cell, draw rectangles
    col = recycle.with.levels(col, le)
    border = recycle.with.levels(border, le)
    lty = recycle.with.levels(lty, le)
    lwd = recycle.with.levels(lwd, le)
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
        
        circos.rect(nx[j-1], 0, nx[j], ny[j],
                    sector.index = le[i], track.index = track.index,
                    col = col[i], border = border[i], lty = lty[i], lwd = lwd[i])
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
# -col            Filled color
# -border         Border color
# -lwd            Line width
# -lty            Line style
#
# == details
# If the interval between ``start`` and ``end`` (larger or equal to 360 or smaller or equal to -360)
# it would draw a full circle or ring. If ``rou2`` is set, it would draw part of a ring.
#
draw.Rcsssector = function(start.degree = 0, end.degree = 360, rou1 = 1, rou2 = NULL,
  center = c(0, 0), clock.wise = TRUE, col = NA, border = "black", lwd = par("lwd"), 
  lty = par("lty")) {

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
    polygon(m, col = col, border = border, lwd = lwd, lty = lty)
  } else {
    m1 = polar2Cartesian(d1)
    m2 = polar2Cartesian(d2)
    
    if(is.circular(start.degree, end.degree)) {  # a ring
      m = rbind(m1, m2[rev(seq_len(nrow(m2))), ,drop = FALSE])
      
      m[, 1] = m[, 1] + center[1]
      m[, 2] = m[, 2] + center[2]
      polygon(m, col = col, border = NA, lwd = 0.1)
      ## two borders
      ##lines(m1[, 1]+center[1], m1[, 2]+center[2], col = "white", lwd = lwd, lty = 1)
      ##lines(m2[, 1]+center[1], m2[, 2]+center[2], col = "white", lwd = lwd, lty = 1)
      lines(m1[, 1]+center[1], m1[, 2]+center[2], col = border, lwd = lwd, lty = lty)
      lines(m2[, 1]+center[1], m2[, 2]+center[2], col = border, lwd = lwd, lty = lty)
      
    } else {
      m = rbind(m1, m2)
      
      m[, 1] = m[, 1] + center[1]
      m[, 2] = m[, 2] + center[2]
      polygon(m, col = col, border = border, lwd = lwd, lty = lty)
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
# -... pass to `circos.text`
#
# == details
# You can use `circos.info` to find out index for all sectors and all tracks.
#
# The function calls `draw.sector`.
highlight.Rcsssector = function(sector.index, track.index = get.all.track.index(), 
  col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
  padding = c(0, 0, 0, 0), text = NULL, text.col = par("col"), 
  text.vjust = 0.5, ...) {

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
#
# == details
# Assuming there are ``n`` nodes in the dendrogram, the positions for leaves on x-axis is ``0.5, 1.5, ..., n - 0.5``.
# So you must be careful with ``xlim`` when you initialize the cirular layout.
#
# You can use the ``dendextend`` package to render the dendrograms.
# 
circos.Rcssdendrogram = function(dend, facing = c("outside", "inside"), max_height = NULL) {

  warning("Rcss features not implemented yet")
  
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
  
  lines_par = function(col = par("col"), lty = par("lty"), lwd = par("lwd"), ...) {
    return(list(col = col, lty = lty, lwd = lwd))
  }
  
  points_par = function(col = par("col"), pch = par("pch"), cex = par("cex"), ...) {
    return(list(col = col, pch = pch, cex = cex))
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
    
    ## graphic parameter for current branch
    ## only for lines, there are lwd, col, lty
    edge_par1 = do.call("lines_par", as.list(attr(d1, "edgePar")))  # as.list to convert NULL to list()
    edge_par2 = do.call("lines_par", as.list(attr(d2, "edgePar")))
    node_par = attr(dend, "nodePar")
    if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(dend, "nodePar")))
    
    ## plot the connection line
    if(facing == "outside") {
      circos.lines(c(x1, x1), max_height - c(y1, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd, straight = TRUE)
      circos.lines(c(x1, (x1+x2)/2), max_height - c(height, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd)
      circos.lines(c(x2, x2), max_height - c(y2, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd, straight = TRUE)
      circos.lines(c(x2, (x1+x2)/2), max_height - c(height, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd)
      if(!is.null(node_par)) {
        circos.points((x1+x2)/2, max_height - height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
      }
    } else if(facing == "inside") {
      circos.lines(c(x1, x1), c(y1, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd, straight = TRUE)
      circos.lines(c(x1, (x1+x2)/2), c(height, height), col = edge_par1$col, lty = edge_par1$lty, lwd = edge_par1$lwd)
      circos.lines(c(x2, x2), c(y2, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd, straight = TRUE)
      circos.lines(c(x2, (x1+x2)/2), c(height, height), col = edge_par2$col, lty = edge_par2$lty, lwd = edge_par2$lwd)
      if(!is.null(node_par)) {
        circos.points((x1+x2)/2, height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
      }
    }
    
    ## do it recursively
    if(is.leaf(d1)) {
      node_par = attr(d1, "nodePar")
      if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(d1, "nodePar")))
      if(facing == "outside") {
        if(!is.null(node_par)) {
          circos.points(x1, max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
        }
      } else if(facing == "inside") {
        if(!is.null(node_par)) {
          circos.points(x1, 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
        }
      }
    } else {
      draw.d(d1, max_height, facing, max_width)
    }
    
    if(is.leaf(d2)) {
      node_par = attr(d2, "nodePar")
      if(!is.null(node_par)) node_par = do.call("points_par", as.list(attr(d2, "nodePar")))
      if(facing == "outside") {
        if(!is.null(node_par)) {
          circos.points(x2, max_height, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
        }
      } else if(facing == "inside") {
        if(!is.null(node_par)) {
          circos.points(x2, 0, col = node_par$col, pch = node_par$pch, cex = node_par$cex)
        }
      }
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

