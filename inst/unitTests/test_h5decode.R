suppressMessages({
    library(rhdf5)
    library(IRanges)
})

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

## verify that `\001NULL\001` is the representation used by
## methods:::.pseudoNULL for the pseudoNULL object; pseudoNULL is used
## as a placeholder symbol for *ORNULL slots in S4 objects
test_pseudoNULL_represenation <- function() {
    checkIdentical(h5robj:::.pseudoNULL, methods:::.pseudoNULL)
}
##test_pseudoNULL_represenation()

test_h5ls_immediate_descendants <- function() {
    ff <- .factor()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(ff, h5fl, top_name)
    H5close()

    descs <- h5robj:::h5ls_immeditate_descendants(h5fl, "foo/attrs")
    tar_descs <- c("/foo/attrs/class", "/foo/attrs/levels")
    checkIdentical(tar_descs, descs)
}
##test_h5ls_immediate_descendants()

test_decode_NULLobj <- function() {
    x <- NULL

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(x, h5fl, top_name)
    H5close()
    
    res <- decode(h5fl, top_name)
    checkIdentical(x, res)
}
##test_decode_NULLobj()

## needed to ensure 'numeric' vs. 'double' is used correctly in decoding
test_decode_zero_length_numeric <- function() {
    nums <- vector('numeric')

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(nums, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(nums, res)
}
##test_decode_zero_length_numeric()

test_decode_primitive <- function() {
    ints <- 1:3

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(ints, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
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

    encode(ints, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
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

    encode(l, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
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

    encode(l, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
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

    encode(l, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(l, res)
}
##test_decode_list_partially_named()

test_decode_list_unnamed <- function() {
    l <- list(1:3, letters[5:7])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(l, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(l, res)
}
##test_decode_list_unnamed()

test_decode_factor <- function() {
    ff <- .factor()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(ff, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
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

    encode(mx, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(mx, res)
}
##test_decode_matrix()

test_decode_SYMSXP <- function() {
    n <- as.name("\001NULL\001")

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(n, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(n, res)
}
##test_decode_SYMSXP()

test_decode_IRanges <- function() {
    ir <- .simpleIRanges()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(ir, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(ir, res)
}
##test_decode_IRanges()

## a very simple S4 object
## FIX ME: use something in infrastructure package
test_decode_PileupParam <- function() {
    pp <- Rsamtools:::PileupParam()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    encode(pp, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(pp, res)
}
##test_decode_PileupParam()

test_decode_adhocS4_ISA_integer <- function() {
    .A = setClass("A", contains="integer")
    a <- .A(1:5)

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"
    encode(a, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(a, res)
}
##test_decode_adhocS4_ISA_integer()

test_decode_adhocS4_HASA_integer <- function() {
    .A = setClass("A", representation(a="integer"))
    a <- .A(a=1:5)

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"
    
    encode(a, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(a, res)
}
##test_decode_adhocS4_HASA_integer()

test_decode_adhocS4_ISA_list <- function() {
    .A = setClass("A", contains="list")
    a <- .A(list(x=1:3, y=letters[8:10]))

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"
    encode(a, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(a, res)
}
##test_decode_adhocS4_ISA_list()

## list-like thing (typeof() still returns "list" for bookkeeping)
test_decode_adhocS4_ISA_data.frame <- function() {
    .A = setClass("A", contains="data.frame")
    a <- .A(data.frame(x=1:3, y=letters[8:10]))

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"
    encode(a, h5fl, top_name)
    H5close()

    res <- decode(h5fl, top_name)
    checkIdentical(a, res)
}
##test_decode_adhocS4_ISA_data.frame()
