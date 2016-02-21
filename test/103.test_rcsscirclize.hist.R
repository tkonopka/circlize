## Tests for circos.Rcsshist functions

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
cc = Rcss("inst/extdata/style3.Rcss")



## ############################################################################

## Initialize
circos.Rcssinitialize(factors=a$factor, x=a$x, Rcss=cc)

## Track 1
circos.RcsstrackHist(a$factor, a$x, Rcss=cc)

## Track 2
circos.RcsstrackHist(a$factor, a$x, bg.col = bgcol[c(1,4,5,6)], col = "#ccccff", Rcss=cc)

## Track 3
circos.RcsstrackHist(a$factor, a$x, bg.col = bgcol, col = NA, Rcss=cc)

circos.clear()
