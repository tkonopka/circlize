mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]

library(circlize)
par(mar = c(1, 1, 1, 1))
chordDiagram(mat, directional = TRUE, transparency = 0.5)
