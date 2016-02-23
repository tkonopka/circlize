## ##################################################################
##
## This is linkRcss.R (circlize package)
## This file contains function for plotting links using Rcssplot
##
## (Functions with commented definition and empty body
## are included to mirror file link.R. But they bodies are removed
## because they do not need re-implementation for Rcssplot)
##
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
  rou = get_most_inside_radius(), rou1 = rou, rou2 = rou, directional = 0,
  Rcss="default", Rcssclass=c(), ...) {

  ## fill in data from Rcss into the args
  link.default = list(h = 1, w = 1, h2 = 1, w2 = 1,
    col = "#000000", lwd = par("lwd"), lty = par("lty"), border = NA,
    arr.length = 0.4,
    arr.width = 0.2, arr.type = "triangle", arr.lty = par("lty"), 
    arr.lwd = par("lwd"), arr.col = "#000000")
  args = RcssFromArgs(list(...), link.default, Rcss, "circlizelink", Rcssclass)
  args = args[names(args) %in% names(link.default)]
  ## end lookup with Rcss

  ## here just call the old function... (lame implementation)
  do.call(circos.link,
          c(list(sector.index1=sector.index1, point1=point1,
                 sector.index2=sector.index2, point2=point2,
                 rou = rou, rou1=rou1, rou2=rou2, directional=directional), args))
    
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
