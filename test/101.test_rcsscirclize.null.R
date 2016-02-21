## Tests for circos.Rcss functions when Rcss style is not specified (NULL)

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




## ############################################################################

## Initialize
circos.Rcssinitialize(factors=a$factor, x=a$x)

## Track 1
circos.RcsstrackPlotRegion(factors=a$factor, y=a$y, 
                           panel.fun = function(x,y) {
                             circos.Rcssaxis()
                           })
circos.RcsstrackPoints(a$factor, a$x, a$y, col = col[1:4])
circos.Rcsstext(-1, 0.5, "left", sector.index = "a", track.index = 1)
circos.Rcsstext(1, 0.5, "right", sector.index = "a")

## Track 2
circos.RcsstrackHist(a$factor, a$x, bg.col = bgcol, col = NA)

## Track 3
circos.RcsstrackPlotRegion(factors = a$factor, x = a$x, y = a$y, 
                           panel.fun = function(x, y) {
                             sector.index = get.cell.meta.data("sector.index")
                             xlim = get.cell.meta.data("xlim")
                             ylim = get.cell.meta.data("ylim")
                             circos.Rcsstext(mean(xlim), mean(ylim), sector.index)
                             circos.Rcsspoints(x[1:10], y[1:10])
                             circos.Rcsspoints(x[11:20], y[11:20])
                           })
circos.RcssupdatePlotRegion(sector.index = "d", track.index = 2)
circos.Rcsspoints(x = -2:2, y = rep(0, 5))
xlim = get.cell.meta.data("xlim")
ylim = get.cell.meta.data("ylim")
circos.Rcsstext(mean(xlim), mean(ylim), "updated")

## Track 4
circos.RcsstrackPlotRegion(factors = a$factor, y = a$y)
circos.RcsstrackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")

## Inner Links
circos.Rcsslink("a", 0, "b", 0, h = 0.4)
circos.Rcsslink("c", c(-0.5, 0.5), "d", c(-0.5,0.5))
circos.Rcsslink("e", 0, "g", c(-1,1), col = "green")

circos.clear()
