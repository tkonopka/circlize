df = read.table(textConnection("
 Brand_from model_from Brand_to Model_to
      VOLVO        s80      BMW  5series
        BMW    3series      BMW  3series
      VOLVO        s60    VOLVO      s60
      VOLVO        s60    VOLVO      s80
        BMW    3series     AUDI       s4
       AUDI         a4      BMW  3series
       AUDI         a5     AUDI       a5
"), header = TRUE, stringsAsFactors = FALSE)

from = paste(df[[1]], df[[2]], sep = ",")
to = paste(df[[3]], df[[4]], sep = ",")

mat = matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
rownames(mat) = unique(from)
colnames(mat) = unique(to)
for(i in seq_along(from)) mat[from[i], to[i]] = 1

circos.par(gap.degree = c(2, 2, 8, 2, 8, 2, 8))

par(mar = c(1, 1, 1, 1))
chordDiagram(mat, order = sort(union(from, to)),
    direction = TRUE, annotationTrack = "grid", preAllocateTracks = list(
        list(track.height = 0.02),
        list(track.height = 0.02))
)
