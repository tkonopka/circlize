## Tests for circos.Rcss functions with base Rcss style

library("GlobalOptions")
library("Rcssplot")

source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")
source("R/globalRcss.R")
source("R/plotRcss.R")
source("R/plotRcss2.R")
source("R/utilsRcss.R")
source("R/linkRcss.R")

## load some test data objects (a, n, col, bgcol)
source("test/test_data/01.test_data.intro.R")

## test Rcss style
cc = Rcss("inst/extdata/circlize.base.Rcss")



## ############################################################################

## Initialize
circos.Rcssinitialize(factors=a$factor, x=a$x, Rcss=cc)

## Track 1
circos.RcsstrackPlotRegion(factors=a$factor, y=a$y, Rcss=cc, 
                           panel.fun = function(x,y) {
                             circos.Rcssaxis(Rcss=cc)
                           })
circos.RcsstrackPoints(a$factor, a$x, a$y, col = col[1:4], Rcss=cc)
circos.Rcsstext(-1, 0.5, "left", sector.index = "a", track.index = 1, Rcss=cc)
circos.Rcsstext(1, 0.5, "right", sector.index = "a", font=2, col="blue", Rcss=cc)

## Track 2
circos.RcsstrackHist(a$factor, a$x, bg.col = bgcol, col = NA, Rcss=cc)

## Track 3
circos.RcsstrackPlotRegion(factors = a$factor, x = a$x, y = a$y, Rcss=cc, 
                           panel.fun = function(x, y) {
                             sector.index = get.cell.meta.data("sector.index")
                             xlim = get.cell.meta.data("xlim")
                             ylim = get.cell.meta.data("ylim")
                             circos.Rcsstext(mean(xlim), mean(ylim), sector.index, Rcss=cc, Rcssclass=c("means"))
                             circos.Rcsspoints(x[1:10], y[1:10], Rcss=cc, Rcssclass=c("first"))
                             circos.Rcsspoints(x[11:20], y[11:20], Rcss=cc, Rcssclass=c("second"))
                           })
circos.RcssupdatePlotRegion(sector.index = "d", track.index = 2, Rcss=cc)
circos.Rcsspoints(x = -2:2, y = rep(0, 5), Rcss=cc, Rcssclass=c("updated"))
xlim = get.cell.meta.data("xlim")
ylim = get.cell.meta.data("ylim")
circos.Rcsstext(mean(xlim), mean(ylim), "updated", Rcss=cc, Rcssclass=c("updated"))

## Track 4
circos.RcsstrackPlotRegion(factors = a$factor, y = a$y, Rcss=cc)
circos.RcsstrackLines(a$factor[1:20], a$x[1:20], a$y[1:20], type = "h", Rcss=cc)

## Inner Links
circos.Rcsslink("a", 0, "b", 0, h = 0.4, Rcss=cc, Rcssclass=c("linkA"))
circos.Rcsslink("c", c(-0.5, 0.5), "d", c(-0.5,0.5), Rcss=cc, Rcssclass=c("linkB"))
circos.Rcsslink("e", 0, "g", c(-1,1), col = "green", Rcss=cc, Rcssclass=c("linkC"))

circos.clear()
