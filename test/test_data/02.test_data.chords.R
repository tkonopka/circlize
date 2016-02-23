## some test data for chord diagrams
## (copied from chord diagram vignette)
##

set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
    to = rep(colnames(mat), each = nrow(mat)),
    value = as.vector(mat),
    stringsAsFactors = FALSE)
df = df[sample(18, 10), ]
df
