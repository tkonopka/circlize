## Tests for circos.Rcss functions with base Rcss style

library("GlobalOptions")
library("Rcssplot")

source("R/global.R")
source("R/plot.R")
source("R/utils.R")
source("R/link.R")
source("R/chordDiagram.R")
source("R/globalRcss.R")
source("R/plotRcss.R")
source("R/plotRcss2.R")
source("R/utilsRcss.R")
source("R/linkRcss.R")
source("R/chordDiagramRcss.R")

## load some test data objects (a, n, col, bgcol)
source("test/test_data/02.test_data.chords.R")

## test Rcss style
cc = Rcss("inst/extdata/style6.Rcss")



## ############################################################################


pdf(file="test/105.pdf", width=4, height=4)
par(mfrow=c(2,2))
chordDiagram(df)
RcsschordDiagram(df, link.lwd=0.3, grid.col=c(S2="#dddd00", E4="#1144cc"), col="grid", Rcss=cc)
RcsschordDiagram(df, link.lwd=0.3, grid.col="random", col=NULL, Rcss=cc)
RcsschordDiagram(df, grid.col="random", col="#ff0000", Rcss=cc)
circos.clear()
dev.off()
