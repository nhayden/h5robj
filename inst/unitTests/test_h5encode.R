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
    top_name <- "foo"
    
    rhdf5:::encode(ir, h5fl, top_name)

    ## test start
    path <- paste(top_name, "start", "data", sep="/")
    start <- as.integer(h5read(h5fl, path))
    checkIdentical(start(ir), start)
    
    ## test width
    path <- paste(top_name, "width", "data", sep="/")
    width <- as.integer(h5read(h5fl, path))
    checkIdentical(width(ir), width)

    ## test NAMES
    ## XXXX FIX ME: what to do??
    path <- paste(top_name, "NAMES", "data", sep="/")
    NAMES <- as.name(h5read(h5fl, path))
    checkIdentical(as.name('\001NULL\001'), NAMES)

    ## test elementMetadata / mcols
    ## XXXX FIX ME: what to do?
    path <- paste(top_name, "elementMetadata", "data", sep="/")
    elementMetadata <- as.name(h5read(h5fl, path))
    checkIdentical(as.name('\001NULL\001'), elementType)

    ## test elementType
    path <- paste(top_name, "elementType", "data", sep="/")
    elementType <- as.character(h5read(h5fl, path))
    checkIdentical(elementType(ir), elementType)

    ## test metadata?
    ## XXXX FIX ME: check for absence of dataset called 'metadata'?

    ## test
    
}
test_write_IRanges()
