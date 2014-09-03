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

.matrix <- function() {
    mdat <- matrix(1, dimnames=list("a", Col="b"))
}

.simpleIRanges <- function() {
    ## from IRanges man page
    IRanges(Rle(1:30) %% 5 <= 2)
}

test_h5ls_immediate_descendants <- function() {
    ff <- .factor()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ff, h5fl, top_name)
    H5close()

    descs <- rhdf5:::h5ls_immeditate_descendants(h5fl, "foo/attrs")
    tar_descs <- c("/foo/attrs/class", "/foo/attrs/levels")
    checkIdentical(tar_descs, descs)
}
##test_h5ls_immediate_descendants()

##trace(rhdf5:::.decode.default, browser)
##trace(rhdf5:::decode_S3, browser)
##trace(rhdf5:::decode_attrs, browser)

test_decode_primitive <- function() {
    ints <- 1:3

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ints, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(ints, res)
}
##test_decode_primitive()

test_decode_primitive_with_attrs <- function() {
    ints <- 1:3
    attributes(ints) <- list(a=8:10, b=letters[1:3])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ints, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(ints, res)
}
##test_decode_primitive_with_attrs()

test_decode_empty_list <- function() {
    l <- list()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(l, res)
}
##test_decode_empty_list()

test_decode_list_named <- function() {
    l <- list(y=c("x", ""), z=letters[5:7])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(l, res)
}
##test_decode_list_named()

test_decode_list_partially_named <- function() {
    l <- list(y=c("x", ""), letters[1:3])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(l, res)
}
##test_decode_list_partially_named()

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
    checkIdentical(ff, res)
}
##test_decode_factor()

test_decode_matrix <- function() {
    mx <- .matrix()
    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(mx, h5fl, top_name)
    H5close()

    res <- rhdf5:::decode(h5fl, top_name)
    checkIdentical(mx, res)
}
##test_decode_matrix()

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
test_decode_IRanges()
