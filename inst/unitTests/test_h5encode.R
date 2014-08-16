suppressMessages({
    library(rhdf5)
    library(GenomicRanges)
    library(RUnit)
})

.gen_timestamp_string <- function() {
    as.character(as.numeric(as.POSIXct(Sys.time())))
}

.simpleIRanges <- function() {
    ## from IRanges man page
    IRanges(Rle(1:30) %% 5 <= 2)
}

trace(rhdf5:::encode, quote(print(obj)))

test_write_IRanges <- function() {
    ir <- .simpleIRanges()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    
    rhdf5:::encode(ir, h5fl, "foo")
}
test_write_IRanges()
