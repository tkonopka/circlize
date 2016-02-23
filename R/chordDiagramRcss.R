
# == title
# Plot Chord Diagram
#
# == param
# -x a matrix or a data frame. The function will pass all argument to `chordDiagramFromMatrix` or `chordDiagramFromDataFrame` depending on the type of ``x``
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              graphics settings passed to `chordDiagramFromDataFrame`.
#
# == details
# Chord diagram is a way to visualize numeric tables ( http://circos.ca/intro/tabular_visualization/ ), especially useful
# when the table represents information of directional relations. This function
# visualize tables in a circular way.
#
# This function is flexible and contains some settings that may be a little difficult to understand. 
# Please refer to vignette for better explanation.
#
# Settings for Rcss:
#   all processing is passed on to RcsschordDiagramFromDataFrame.
#   See documentation for those functions.
#
RcsschordDiagram = function(x, Rcss="default", Rcssclass=c(), ...) {
  
  if(inherits(x, "matrix")) {
    stop("RcsschordDiagam does not support plotting from a matrix.")
    ##RcsschordDiagramFromMatrix(x, Rcss=Rcss, Rcssclass=Rcssclass, ...)
  } else if(inherits(x, "data.frame")) {
    if(ncol(x) > 3) {
      if(all(sapply(x, inherits, "numeric"))) {
        warning("It seems your input data is an adjacency matrix, maybe you need to convert it to 'matrix' explicitely.")
      }
    }
    RcsschordDiagramFromDataFrame(x, Rcss=Rcss, Rcssclass=Rcssclass, ...)
  } else {
    stop("`x` can only be a matrix or a data frame.")
  }  
}





# parsePreAllocateTracksValue not used in Rcss (?)
##parsePreAllocateTracksValue = function(preAllocateTracks) { }





# .normalize_to_mat does not need changing to Rcss
##.normalize_to_mat = function(value, rn, cn, default) { }






# title
# Adjust gaps to make chord diagrams comparable
#
# == param
# -mat1           matrix that has the largest sum of absolute
# -mat2           matrix to be compared
# -Rcss           Rcss style object
# -Rcssclass      sub class for style sheet
# -...            graphical parameters 
#
# == details
# Normally, in Chord Diagram, values in mat are normalized to the summation and each value is put 
# to the circle according to its percentage, which means the width for each link only represents 
# kind of relative value. However, when comparing two Chord Diagrams, it is necessary that unit 
# width of links in the two plots should be represented in a same scale. This problem can be solved by 
# adding more blank gaps to the Chord Diagram which has smaller values.
#
# == value
# Sum of gaps for ``mat2``.
#
# This function considers the "circlize" and extracts gap.degree and start.degree
#
RcssnormalizeChordDiagramGap = function(mat1, mat2, Rcss="default", Rcssclass=c(), ...) {
  
  percent = sum(abs(mat2)) / sum(abs(mat1))

  ## look up gap.degree and start.degree from args or Rcss
  gap.defaults = list(gap.degree=1)
  args = RcssFromArgs(list(...), gap.defaults, Rcss, "circlize", Rcssclass)
  ## end of Rcss lookup
  
  if(length(gap.degree) == 1) {
    gap.degree = rep(args[["gap.degree"]], length(unique(rownames(mat1), colnames(mat1))))
  }
  blank.degree = (360 - sum(args[["gap.degree"]])) * (1 - percent)  
  return(blank.degree)
}





## mat2df does not need changing from Rcss
## mat2df = function(mat) { }





## In Rcss implementation, plotting from a matrix is not supported (keep it simple...)
## RcsschordDiagramFromMatrix = function() {} 





# == title
# Plot Chord Diagram from a data frame
#
# == param
# -df A data frame with at least two columns. The first two columns specify the connections and the third column (optional)
#     contains numeric values which are mapped to the width of links as well as the colors if ``col`` is specified as a color mapping function.
#     The sectors in the plot will be ``union(df[[1]], df[[2]])``.
# -order Order of sectors. Default order is ``union(df[[1]], df[[2]])``.
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
#-... pass to       custom graphics parameters
#
# (Got rid of preAllocateTracks - what does this do?)
# preAllocateTracks Pre-allocate empty tracks before drawing Chord diagram. It can be a single number indicating
#                    how many empty tracks needed to be created or a list containing settings for empty
#                    tracks. Please refer to vignette for details.
#
RcsschordDiagramFromDataFrame = function(df, order = NULL, Rcss="default", Rcssclass=c(), ...) {

  ## copy of settings from old function definition into an internal list here
  chord.default = list(grid.col = NULL, grid.border=NULL, transparency = 0.5, col = NULL,
    directional = 0, direction.type = "diffHeight", diffHeight = 0.04, reduce = 1e-5, self.link = 2,
    annotationTrack = c("name", "grid", "axis"), annotationTrackHeight = c(0.05, 0.05), 
    link.sort = FALSE, link.decreasing = TRUE)
  chord.default2 = list(link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"), 
    link.arr.length = 0.4, 
    link.arr.width = 0.4/2, link.arr.type = "triangle", link.arr.lty = par("lty"), 
    link.arr.lwd = par("lwd"), link.arr.col = par("col"))
  
  ## lookup data from Rcss or from args
  nowargs = RcssFromArgs(list(...), chord.default, Rcss, "chorddiagram", Rcssclass)
  nowargs = nowargs[names(nowargs) %in% names(chord.default)]
  ## end Rcss lookup
  
  ## get a list of arguments not used by the chord diagram itself (i.e. link settings)
  otherargs = list(...)
  otherargs = otherargs[!(names(otherargs) %in% names(chord.default))]
  ## get a list of arguments for Rcsslink()
  linkargs = otherargs[grepl("^link.", names(otherargs))]
  names(linkargs) = substr(names(linkargs), 6, nchar(names(linkargs)))
  
  if(nrow(df) != 2) {
    if(identical(nowargs$direction.type, c("diffHeight", "arrows")) || identical(nowargs$direction.type, c("arrows", "diffHeight"))) {
      nowargs$direction.type = "diffHeight+arrows"
    }
  }

  ## check the format of the data frame
  if(!inherits(df, "data.frame")) {
    stop("`df` must be a data frame.")
  }
  if(ncol(df) < 2) {
    stop("`df` should have at least have two columns.")
  }
  if(ncol(df) == 2) {
    df$value = rep(1, nrow(df))
  }
  df = df[1:3]
  df[[1]] = as.character(df[[1]])
  df[[2]] = as.character(df[[2]])
  colnames(df) = c("rn", "cn", "value")
  nr = nrow(df)

  ## handle transparency
  transparency = nowargs$transparency
  transparency = ifelse(transparency < 0, 0, ifelse(transparency > 1, 1, transparency))
  ## handle ordering of the categories
  cate = union(df[[1]], df[[2]])
  if(!is.null(order)) {
    if(length(order) != length(cate)) {
      stop("Length of `order` should be same as number of sectors.")
    }
    if(is.numeric(order)) {
      if(!setequal(order, seq_along(cate))) {
        stop(paste0("`order` needs to be integers ranging from 1 to", length(cate)))
      }
      cate = cate[order]
    } else {
      if(!setequal(order, cate)) {
        stop("`order` should only be picked from sectors.")	
      }
      cate = order
    }
  }
  n = length(cate)

  ## handle colors for outer ring etc
  Rcss.grid = Rcss ## styles for outer ring
  Rcss.chords = Rcss ## styles for chords
  
  grid.col = nowargs$grid.col
  grid.border = nowargs$grid.border;
  if (identical(grid.col, "NA") | identical(grid.col, "NULL") | identical(grid.col, NA)) grid.col=NULL
  if (identical(grid.border, "NA") | identical(grid.border, "NULL") | identical(grid.border, NA)) grid.border=NULL
  
  if (is.null(grid.col)) {
    ## in Rcss version, do nothing here, let Rcss for polygons take over
  } else {
    if(length(grid.col)==1) {
      if (grid.col=="random") {
        grid.col = rand_color(n)
        names(grid.col) = cate
      } else {
        if (is.null(names(grid.col))) {
          grid.col = rep(grid.col, n)
          names(grid.col) = cate
        }
      }
    } else if (length(grid.col) == length(cate)) {
      if (is.null(names(grid.col))) {
        names(grid.col) = cate
      }
    } 
  }
  if (is.null(grid.border)) {
    ## in Rcss version, do nothing here, let Rcss for polygons take over
    grid.border = NA;
  } else {
    if (length(grid.border)==1) {
      if (grid.border=="random") {
        grid.border=grid.col;
      } else {
        if (is.null(names(grid.border))) {
          grid.border = rep(grid.border, n)
          names(grid.border) = cate
        }
      }
    } else if (length(grid.border) == length(cate)) {
      if (is.null(names(grid.border))) {
        names(grid.border) = cate
      }
    }
  }
  if (!is.null(grid.col)) {
    Rcss.grid = updateRcss(Rcss.grid, cate, list(col=grid.col), c("col"), "polygon", Rcssclass)
  }
  if (!is.null(grid.border)) {
    Rcss.grid = updateRcss(Rcss.grid, cate, list(border=grid.border), c("border"), "polygon", Rcssclass)
  }
  
  ## colors for the chords/links
  col = nowargs$col
  if(is.null(col)) {
    ## In Rcss, not setting col means deferring to css
  } else {
    if (identical(col, "grid")) {
      ## get colors for each link from the grid
      col = rep(NA, nr)
      if (nrow(df)>0) {
        ## set colors for links originating from each sector based on Rcss
        for(k in unique(df[,1])) {
          col[df[,1]==k] = RcssGetPropertyValueOrDefault(Rcss.grid, "polygon", "col", default=NA, c(Rcssclass, k))
        }
        ## update the colors using the custom grid.col objects
        for (k in seq_along(grid.col)) {
          col[df[,1]==names(grid.col)[k]] = grid.col[k]
        }
      }
    } else if (is.function(col)) {
      col = col(df[[3]])
    } else {
      col = rep(col, nr)[1:nr]
    }    
    rgb_mat = t(col2rgb(col, alpha = TRUE))
    if(is.na(transparency)) {
      col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
    } else if(all(rgb_mat[, 4] == 255)) {
      col = rgb(rgb_mat, maxColorValue = 255, alpha = (1-transparency)*255)
    } else {
      col = rgb(rgb_mat, maxColorValue = 255, alpha = rgb_mat[, 4])
    }
  }
  ## update colors for all links in the chords Rcss
  if (!is.null(col)) {
    for(k in seq_len(nrow(df))) {
      nowfrom = df[k, 1]
      nowto = df[k, 2]
      Rcss.chords = RcssChangePropertyValue(Rcss.chords, selector="circlizelink", Rcssclass=c("chords", Rcssclass, nowfrom, nowto),
        property="col", value=col[k])     
    }
  }
  
  linkargs = lapply(linkargs, function(x) { rep(x, nr)[1:nr] } )
  directional = rep(nowargs$directional, nr)[1:nr]
  direction.type = rep(nowargs$direction.type, nr)[1:nr]
  diffHeight = rep(nowargs$diffHeight, nr)[1:nr]

  ## reduce the data frame
  xsum = structure(rep(0, length(cate)), names = cate)
  for(i in seq_len(nr)) {
    if(df$rn[i] == df$cn[i]) {
      xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])
      if(self.link == 2) {
        xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])  # <<- self-link!!!!!
      }
    } else {
      xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])
      xsum[df$cn[i]] = xsum[df$cn[i]] + abs(df$value[i])
    }
  }

  reduce = nowargs$reduce;
  keep = names(xsum)[xsum / sum(xsum) >= reduce]
  l = df$rn %in% keep | df$cn %in% keep

  ## get rid of some rows in the data frame (and accompanying settings)
  cate = intersect(cate, keep)
  df = df[l, , drop = FALSE]
  ##grid.col = grid.col[intersect(names(grid.col), keep)]
  ##col = col[l]
  linkargs = lapply(linkargs, function(x) {x[l]})
  directional = directional[l]
  direction.type = direction.type[l]
  diffHeight = diffHeight[l]
  
  nr = nrow(df)
  ## re-calcualte xsum
  xsum = structure(rep(0, length(cate)), names = cate)
  for(i in seq_len(nr)) {
    if(df$rn[i] == df$cn[i]) {
      xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])
      if(self.link == 2) {
        xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])  # <<- self-link!!!!!
      }
    } else {
      xsum[df$rn[i]] = xsum[df$rn[i]] + abs(df$value[i])
      xsum[df$cn[i]] = xsum[df$cn[i]] + abs(df$value[i])
    }
  }
  
  ## add additinal columns in df
  df$o1 = rep(0, nr)  # order of the link root in the sector
  df$o2 = rep(0, nr)  # order of the other link root in the sector
  df$x1 = rep(0, nr)  # end position of the link root in the sector
  df$x2 = rep(0, nr)  # end position on the other link root in the sector
  
  ## ###### sort links on every sector
  ## row first
  .order = function(x, sort = FALSE, reverse = FALSE) {
    if(sort) {
      od = order(x)
    } else {
      od = seq_along(x)
    }
    if(reverse) {
      od = rev(od)
    }
    return(od)
  }
  
  link.sort = nowargs$link.sort
  if(length(link.sort) == 1) link.sort = rep(link.sort, 2)
  link.decreasing = nowargs$link.decreasing
  if(length(link.decreasing) == 1) link.decreasing = rep(link.decreasing, 2)
  self.link = nowargs$self.link;
  
  ## position of root 1
  od = tapply(abs(df$value), df$rn, .order, link.sort[1], link.decreasing[1])
  for(nm in names(od)) {  # for each sector
    l = df$rn == nm # rows in df that correspond to current sector
    df$o1[l] = od[[nm]] # adjust rows according to the order in current sector
    df$x1[l][od[[nm]]] = cumsum(abs(df$value[l])[od[[nm]]]) # position
    
    l2 = df$rn == nm & df$cn == nm 
    if(sum(l2)) { # there is a self link
      if(self.link == 1) {
        df$x2[l2] = df$x1[l2]+abs(df$value[l2])*0.000001
      }
    }
  }

  max_o1 = sapply(od, max)
  sum_1 = tapply(abs(df$value), df$rn, sum)
  ## position of root 2
  od = tapply(abs(df$value), df$cn, .order, link.sort[2], link.decreasing[2])
  for(nm in names(od)) {
    if(!is.na(max_o1[nm])) { # if cn already in rn
      l = df$cn == nm
      if(self.link == 1) {
        l2 = ! df$rn[l] == nm # self link
        od[[nm]] = order(od[[nm]][l2])
      } else {
        l2 = rep(TRUE, sum(l))
      }
      df$o2[l][l2] = od[[nm]] + max_o1[nm]
      df$x2[l][l2][ od[[nm]] ] = cumsum(abs(df$value[l][l2])[ od[[nm]] ]) + sum_1[nm]
    } else {
      l = df$cn == nm
      df$o2[l] = od[[nm]]
      df$x2[l][od[[nm]]] = cumsum(abs(df$value[l])[od[[nm]]])
    }
  }

  if(self.link == 1) {
    l = df$rn == df$cn
    df$x1[l] = pmin(df$x1[l], df$x2[l])
    df$x2[l] = pmin(df$x1[l], df$x2[l])
  }
  
  ## #####################################
  ## End of prep - now create regions and draw components onto the chart
  
  
  Rcss2 = RcssChangePropertyValue(Rcss, "circlizeregion", property="cell.padding", value=c(0, 0, 0, 0), Rcssclass=Rcssclass)
  circos.Rcssinitialize(factors = factor(cate, levels = cate), xlim = cbind(rep(0, length(xsum)), xsum), Rcss=Rcss2, Rcssclass=Rcssclass)
  
  annotationTrack = nowargs$annotationTrack
  annotationTrackHeight = nowargs$annotationTrackHeight

  if("name" %in% annotationTrack) {
    namelab.line = RcssGetPropertyValueOrDefault(Rcss, "chorddiagram", "namelab.line", default=0.9, Rcssclass=Rcssclass)    
    circos.RcsstrackPlotRegion(ylim = c(0, 1), bg.border = NA,
                               panel.fun = function(x, y) {
                                 xlim = get.cell.meta.data("xlim")
                                 current.sector.index = get.cell.meta.data("sector.index")
                                 i = get.cell.meta.data("sector.numeric.index")
                                 circos.Rcsstext(mean(xlim), namelab.line, labels = current.sector.index,                                                  
                                                 facing = "inside", niceFacing = TRUE, adj = c(0.5, 0),
                                                 Rcss=Rcss, Rcssclass=c("name", Rcssclass))
                               },
                               track.height = annotationTrackHeight[which(annotationTrack %in% "name")],
                               Rcss=Rcss, Rcssclass=Rcssclass)
  }
  if("grid" %in% annotationTrack) {
    circos.RcsstrackPlotRegion(ylim = c(0, 1), bg.border = NA, 
                               panel.fun = function(x, y) {
                                 xlim = get.cell.meta.data("xlim")
                                 current.sector.index = get.cell.meta.data("sector.index")
                                 circos.Rcssrect(xlim[1], 0, xlim[2], 1,
                                                 Rcss=Rcss.grid, Rcssclass=c(Rcssclass, current.sector.index))
                                 if("axis" %in% annotationTrack) {
                                   circos.Rcssaxis("top", Rcss=Rcss, Rcssclass=Rcssclass) 
                                 }
                               },
                               track.height = annotationTrackHeight[which(annotationTrack %in% "grid")],
                               Rcss=Rcss.grid, Rcssclass=Rcssclass)
  }
  
  rou = get_most_inside_radius()
  rou1 = numeric(nr)
  rou2 = numeric(nr)
  for(i in seq_len(nr)) {
    if(directional[i]) {
      if(grepl("diffHeight", direction.type[i])) {
        if(directional[i] == 1) {
          if(diffHeight[i] > 0) {
            rou1[i] = rou - diffHeight[i]
            rou2[i] = rou
          } else {
            rou1[i] = rou
            rou2[i] = rou + diffHeight[i]
          }
        } else if(directional[i] == -1) {
          if(diffHeight[i] > 0) {
            rou1[i] = rou
            rou2[i] = rou - diffHeight[i]
          } else {
            rou1[i] = rou + diffHeight[i]
            rou2[i] = rou
          }
        } else  if(directional[i] == 2) {
          if(diffHeight[i] > 0) {
            rou1[i] = rou - diffHeight[i]
            rou2[i] = rou - diffHeight[i]
          } else {
            rou1[i] = rou + diffHeight[i]
            rou2[i] = rou + diffHeight[i]
          }
        }
      } else {
        rou1[i] = rou
        rou2[i] = rou
      }
    } else {
      rou1[i] = rou
      rou2[i] = rou
    }
  }
  
  for(k in seq_len(nrow(df))) {
    nowfrom = df[k, 1]
    nowto = df[k, 2]
    nowargsk = lapply(linkargs, function(x) {x[k]} )
    do.call(circos.Rcsslink,
            c(list(sector.index1=df$rn[k], point1=c(df$x1[k] - abs(df$value[k]), df$x1[k]),
                   sector.index2=df$cn[k], point2=c(df$x2[k] - abs(df$value[k]), df$x2[k]),
                   rou1= rou1[k], rou2=rou2[k], Rcss=Rcss.chords, Rcssclass=c("chords", Rcssclass, nowfrom, nowto)), nowargsk))        
  }
  
}



##
## psubset = function(mat, ri, ci) { }
