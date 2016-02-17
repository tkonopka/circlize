#source("R/global.R")
#source("R/plot.R")
#source("R/utils.R")
#source("R/link.R")
#library("Rcssplot")
#library("circlize")

## TO DO PROPERLY

if (FALSE) {
cc = Rcss(file.rcss)


set.seed(999)
n = 1000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
    x = rnorm(n), y = runif(n))


par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
par(mfrow=c(1,2));

## using standard circlize

circos.par("track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)

circos.trackPlotRegion(factors = a$factor, y = a$y,
                       panel.fun = function(x, y) {
                         circos.axis()
                       })
if (TRUE) {
  col = rep(c("#FF0000", "#00FF00"), 4)
  circos.trackPoints(a$factor, a$x, a$y, col = col, pch = 16, cex = 0.5)
  circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
}
if (TRUE) {
  bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
  circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)  
}
if (TRUE) {
  circos.text(1, 0.5, "right", sector.index = "a")
  circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
                         panel.fun = function(x, y) {
                           grey = c("#FFFFFF", "#CCCCCC", "#999999")
                           sector.index = get.cell.meta.data("sector.index")
                           xlim = get.cell.meta.data("xlim")
                           ylim = get.cell.meta.data("ylim")
                           circos.text(mean(xlim), mean(ylim), sector.index)
                           circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
                           circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
                         })
}
if (TRUE) {
  circos.updatePlotRegion(sector.index = "d", track.index = 2)
  circos.points(x = -2:2, y = rep(0, 5))
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), "updated")
}
if (TRUE) {
  circos.trackPlotRegion(factors = a$factor, y = a$y)
  circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")
}
if (TRUE) {
  circos.link("a", 0, "b", 0, h = 0.4)
  circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
              border = "blue", h = 0.2)
  circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)
}




## using Rcsscirclize

##circos.Rcsspar(cc)
circos.Rcssinitialize(factors=a$factor, x=a$x, Rcss=cc)

circos.RcsstrackPlotRegion(factors=a$factor, y=a$y, Rcss=cc, 
                           panel.fun = function(x,y) {
                             circos.Rcssaxis(Rcss=cc)
                           })
col = rep(c("#FF0000", "#00FF00"), 4)
names(col) = letters[1:8]
col = col[sample(letters[1:8], 8, replace=FALSE)]
if (TRUE) {
  circos.RcsstrackPoints(a$factor, a$x, a$y, col = col[1:4],
                         Rcss=cc, Rcssclass="circlize")
  circos.Rcsstext(-1, 0.5, "left", sector.index = "a", track.index = 1, Rcss=cc)
  circos.Rcsstext(1, 0.5, "right", sector.index = "a", Rcss=cc)
}
if (TRUE) {
  bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
  circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)  
}
if (TRUE) {
  circos.RcsstrackPlotRegion(factors = a$factor, x = a$x, y = a$y, Rcss=cc, Rcssclass="circlize", 
                             panel.fun = function(x, y) {
                               grey = c("#FFFFFF", "#CCCCCC", "#999999")
                               sector.index = get.cell.meta.data("sector.index")
                               xlim = get.cell.meta.data("xlim")
                               ylim = get.cell.meta.data("ylim")
                               circos.Rcsstext(mean(xlim), mean(ylim), sector.index, Rcss=cc, Rcssclass=c("circlize", "means"))
                               circos.Rcsspoints(x[1:10], y[1:10], Rcss=cc, Rcssclass=c("circlize", "RR"))
                               circos.Rcsspoints(x[11:20], y[11:20], Rcss=cc, Rcssclass=c("circlize", "BB"))
                             })
}
if (TRUE) {
  circos.RcssupdatePlotRegion(sector.index = "d", track.index = 2, Rcss=cc, Rcssclass=c())
  circos.Rcsspoints(x = -2:2, y = rep(0, 5), Rcss=cc, Rcssclass=c("circlize", "updated"))
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.Rcsstext(mean(xlim), mean(ylim), "updated", Rcss=cc, Rcssclass=c("circlize", "updated"))
}
if (TRUE) {
  circos.RcsstrackPlotRegion(factors = a$factor, y = a$y, Rcss=cc, Rcssclass="circlize")
  circos.RcsstrackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h", Rcss=cc, Rcssclass="circlize")
}
if (TRUE) {
  circos.Rcsslink("a", 0, "b", 0, h = 0.4, Rcss=cc, Rcssclass=c())
  circos.Rcsslink("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
                  border = "blue", h = 0.2, Rcss=cc, Rcssclass=c())
  circos.Rcsslink("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2, Rcss=cc, Rcssclass=c())
}
}
