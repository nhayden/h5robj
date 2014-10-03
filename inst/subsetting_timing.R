suppressMessages({
    library(rhdf5)
    library(h5robj)
    library(bit)
    ##library(IRanges)
})

idx_sample <- function(max, n, density) {
    densities <- vector('list', n)
    num_idx <- max * density
    for(i in seq_len(n)) {
        densities[[i]] <- sort(sample(seq_len(max), num_idx))
    }
    densities
}
##print(lapply(seq(.2, .4, .1), function(x) idx_sample(10, 5, x)))

