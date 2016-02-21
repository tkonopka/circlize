## Tests for dendrogram circos.Rcss function

library("GlobalOptions")
library("Rcssplot")
library("colorspace")

source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")
source("R/globalRcss.R")
source("R/plotRcss.R")
source("R/plotRcss2.R")
source("R/utilsRcss.R")
source("R/linkRcss.R")

## test Rcss style
cc = Rcss("inst/extdata/style5.Rcss")



## ############################################################################
## example from package vignette, adapted to Rcss

set.seed(12345)
n = 80;
r = 8;
mat = matrix(rnorm(n*r), nrow = r, ncol = n) + matrix(rep(seq(-2.2,2.2,length=n), each=r), nrow=r)
col_fun = colorRamp2(c(-2, 0, 2), c("#00dd00", "#000000", "#dd0000"))
factors = rep(letters[1:2], n/2)
maxy=0;

circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.Rcssinitialize(factors, xlim = c(0, n/2), Rcss=cc)

circos.RcsstrackPlotRegion(ylim = c(0, 10), bg.border = NA,
                           panel.fun = function(x, y) {
                             sector.index = get.cell.meta.data("sector.index")
                             m = mat[, factors == sector.index]
                             
                             dend.col = as.dendrogram(hclust(dist(t(m))))                             
                             maxy = ifelse(maxy > attr(dend.col, "height"), maxy, attr(dend.col, "height"))
                             assign("maxy", maxy, envir = .GlobalEnv)
                             m2 = m[, labels(dend.col)]
                             col_mat = col_fun(m2)
                             nr = nrow(m2)
                             nc = ncol(m2)
                             for(i in 1:nr) {
                               for(j in 1:nc) {
                                 circos.Rcssrect(j-1, nr-i, j, nr-i+1, border = col_mat[i, j], col = col_mat[i, j], Rcss=cc)
                               }
                             }                             
                           })

circos.RcsstrackPlotRegion(ylim = c(0, maxy), Rcss=cc, Rcssclass="dendrogram",                           
                           panel.fun = function(x, y) {
                             sector.index = get.cell.meta.data("sector.index")
                             m = mat[, factors == sector.index]                             
                             dend.col = as.dendrogram(hclust(dist(t(m))))
                             if (sector.index=="a") {
                               circos.Rcssdendrogram(dend.col, max_height = maxy, Rcss=cc, Rcssclass="dendrogram")
                             } else {
                               circos.Rcssdendrogram(dend.col, max_height = maxy, col="#0000dd", Rcss=cc, Rcssclass="dendrogram")
                             }
                           })

circos.clear()
