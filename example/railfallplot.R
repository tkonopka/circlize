
## Rainfall plot visualization of CpG islands

library(circlize)
bed1 = generateRandomBed(nr = 2000)
bed2 = generateRandomBed(nr = 2000)

cgi = tempfile(fileext = ".gz")
download.file("http://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/cpgIslandExt.txt.gz", destfile = cgi)
bed = read.table(cgi, sep = "\t")[2:4]

par(mar = c(1, 1, 1, 1))
circos.initializeWithIdeogram()
circos.genomicRainfall(bed, pch = 16, cex = 0.3, col = "#0000FF40")
circos.genomicDensity(bed, col = "#0000FF40")
circos.clear()
