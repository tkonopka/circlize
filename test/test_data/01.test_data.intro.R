## test data and custom objects

set.seed(999)
n = 1000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
    x = rnorm(n), y = runif(n))
col = structure(rep(c("#FF0000", "#00FF00"), 4), names=letters[1:8])
bgcol = structure(rep(c("#EFEFEF", "#CCCCCC"), 4), names=letters[1:8])
