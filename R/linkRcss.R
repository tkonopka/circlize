## ##################################################################
##
## This is linkRcss.R (circlize package)
## This file contains function for plotting links using Rcssplot
##
## (Functions with commented definition and empty body
## are included to mirror file link.R. But they bodies are removed
## because they do not need re-implementation for Rcssplot)
##
## The implementation of circlize.Rcsslink is particularly inelegant.
## Suggestions welcome.
##
## ##################################################################




# == title
# Draw links between points or intervals
#
# == param
# -sector.index1 Index for the first sector
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -sector.index2 Index for the other sector
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -rou           The position of the 'root' of the link. It is the percentage of the radius of the unit circle.
#                By default its value is the position of bottom margin of the most inner track.
# -rou1          The position of root 1 of the link. 
# -rou2          The position of root 2 of the link.
# -h             Height of the link. 
# -w             Since the link is a Bezier curve, it controls the shape of Bezier curve.
# -h2            Height of the bottom edge of the link if it is a ribbon.
# -w2            Shape of the bottom edge of the link if it is a ribbon.
# -directional   0 for no direction, 1 for direction from point1 to point2, -1 for direction from point2 to point1.
#                2 for two directional
# -Rcss             Rcss style object
# -Rcssclass        sub class for style sheet
# -...              Further graphical parameters (see details)
#
# == details
# Links are implemented as quadratic Bezier curves.
#
# Drawing links does not create any track. So you can think it is independent of the tracks.
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# links would look nice. 
#
# See vignette for detailed explanation.
#
#
#  track.height, track.margin, cell.padding, 
# Arguments in "..." processed by circlize: arr.col, arr.lwd, arr.lty, arr.type, arr.width, arr.length
#                                           col, lwd, lty, border,
#                                           h, w, h2, w2
# Arguments in "..." processed by Rcssplot: 
#
circos.Rcsslink = function(sector.index1, point1, sector.index2, point2,
  rou = get_most_inside_radius(),
  rou1 = rou, rou2 = rou, 
  directional = 0,
  Rcss="default", Rcssclass=c(), ...) {
  
  ## start lookup with Rcss
  args = list(...);
  if (!hasArg(col)) {
    col = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "col", Rcssclass=Rcssclass, default="#000000")
  } else {
    col = args[["col"]]
  }
  if (!hasArg(lwd)) {
    lwd = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "lwd", Rcssclass=Rcssclass, default=1)
  } else {
    lwd = args[["lwd"]]
  }
  if (!hasArg(lty)) {
    lty = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "lty", Rcssclass=Rcssclass, default=1)
  } else {
    lty = args[["lty"]]
  }
  if (!hasArg(border)) {
    border = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "border", Rcssclass=Rcssclass, default=NA)
  } else {
    border = args[["border"]]
  }

  args = args[!(names(args) %in% c("col","lwd","lty","border"))]
  
  if (!hasArg(arr.type)) {
    arr.type = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.type", Rcssclass=Rcssclass, default="triangle")
  } else {
    arr.type = args[["arr.type"]]
  }  
  if (!hasArg(arr.length)) {
    arr.length = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.length", Rcssclass=Rcssclass,
      default=ifelse(arr.type=="big.arrow", 0.02, 0.4))
  } else {
    arr.length = args[["arr.length"]]
  }
  if (!hasArg(arr.width)) {
    arr.width = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.width", Rcssclass=Rcssclass, default=arr.length/2)
  } else {
    arr.width = args[["arr.width"]]
  }
  if (!hasArg(arr.lwd)) {
    arr.lwd = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.lwd", Rcssclass=Rcssclass, default=lwd)
  } else {
    arr.lwd = args[["arr.lwd"]]
  }
  if (!hasArg(arr.lty)) {
    arr.lty = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.lty", Rcssclass=Rcssclass, default=lty)
  } else {
    arr.lty = args[["arr.lty"]]
  }
  if (!hasArg(arr.col)) {
    arr.col = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "arr.col", Rcssclass=Rcssclass, default=col)
  } else {
    arr.col = args[["arr.col"]]
  }
  args = args[!(names(args) %in% c("arr.width","arr.length","arr.type", "arr.lwd","arr.lty", "arr.col"))]

  if (!hasArg(h)) {
    h = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "h", Rcssclass=Rcssclass, default=1)
  } else {
    h = args[["h"]]
  }
  if (!hasArg(h2)) {
    h2 = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "h2", Rcssclass=Rcssclass, default=h)
  } else {
    h2 = args[["h2"]]
  }
  if (!hasArg(w)) {
    w = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "w", Rcssclass=Rcssclass, default=1)
  } else {
    w = args[["w"]]
  }
  if (!hasArg(w2)) {
    w2 = RcssGetPropertyValueOrDefault(Rcss, "circlizelink", "w2", Rcssclass=Rcssclass, default=w)
  } else {
    w2 = args[["w2"]]
  }    
  ## end lookup with Rcss

  ## here just call the old function... (lame implementation)
  circos.link(sector.index1, point1, sector.index2, point2,
              rou = rou, rou1=rou1, rou2=rou2, h=h, w=w, h2=h2, w2=2,
              directional=directional,
              col=col, lwd=lwd, lty=lty, border=border,
              arr.width=arr.width, arr.length=arr.length, arr.type=arr.type,
              arr.lwd=arr.lwd, arr.lty=arr.lty, arr.col=arr.col)
  
  
}

##arc.points = function(theta1, theta2, rou) { }

##arc.midpoint = function(theta1, theta2, rou) { }

##getQuadraticPoints = function(theta1, theta2, rou1, rou2, h = NULL, w = 1) { }

##quadratic.bezier = function(p0, p1, p2, w = 1) { }

## quadratic.bezier.length = function(p0, p1, p2, w = 1) { }

## are.lines.intersected = function(x1, y1, x2, y2) { }

## revMat = function(mat) { }



## draws a line with arrowhead
RcsslinesWithArrows = function(d, sep = 6, Rcss="default", Rcssclass=c(), ...) {
  Rcsslines(d, Rcss=Rcss, Rcssclass=Rcssclass, ...)
  if(nrow(d) > sep) {
    for(i in seq(sep, nrow(d)-sep, by = sep)) {
      Rcssarrows(d[i-sep/2, 1], d[i-sep/2, 2], d[i+sep/2, 1], d[i+sep/2, 2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
    }
  } else {
    Rcssarrows(d[1, 1], d[1, 2], d[2, 1], d[2, 2], Rcss=Rcss, Rcssclass=Rcssclass, ...)
  }
}



## degreeDiff = function (theta1, theta2) { }

## degreeDiff2 = function(theta1, theta2) { }

## line_degree = function(x0, y0, x1, y1) { }
