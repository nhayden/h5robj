suppressMessages({
    library(rhdf5)
    library(GenomicRanges)
    library(RUnit)
})

options(error=recover)

.factor <- function() {
    vals <- c("d", "a", "a", "e", "e", "b", "a", "e", "c", "b")
    lvls <- c("a", "b", "c", "d", "e")
    factor(vals, lvls)
}

.simpleIRanges <- function() {
    ## from IRanges man page
    IRanges(Rle(1:30) %% 5 <= 2)
}

test_decode_factor <- function() {
    ff <- .factor()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ff, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    browser()
}
test_decode_factor()

test_decode_IRanges <- function() {
    ir <- .simpleIRanges()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ir, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    browser()
}
##test_decode_IRanges()
