
require(grDevices)

# Setting for histogram -----------------
#' functions for argument "binwidth=..."
bw = function(x, f) ceiling((max(x) - min(x)) / f(x))
FD = function(x) bw(x, nclass.FD)
scott = function(x) bw(x, nclass.scott)
sturges = function(x) bw(x, nclass.Sturges)