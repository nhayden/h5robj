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

.data.frame <- function() {
    data.frame(y=1:10, z=seq(1.1, 10.1))
}

.factor <- function() {
    vals <- c("d", "a", "a", "e", "e", "b", "a", "e", "c", "b")
    lvls <- c("a", "b", "c", "d", "e")
    factor(vals, lvls)
}

.matrix <- function() {
    mdat <- matrix(1, dimnames=list("a", Col="b"))
}

trace(rhdf5:::encode, quote(print(obj)))
trace(rhdf5:::encode_bookkeeping, quote(print(obj)))
trace(rhdf5:::encode_list_like, quote(print(obj)))
options(error=recover)

## XXXX FIX ME: complete
## test_write_IRanges <- function() {
##     ir <- .simpleIRanges()

##     h5fl <- tempfile(fileext=".h5")
##     if(interactive())
##         message(h5fl)
##     h5createFile(h5fl)
##     top_name <- "foo"
    
##     rhdf5:::encode(ir, h5fl, top_name)
##     H5close()

##     ## test start
##     path <- paste(top_name, "start", "data", sep="/")
##     start <- as.integer(h5read(h5fl, path))
##     checkIdentical(start(ir), start)
    
##     ## test width
##     path <- paste(top_name, "width", "data", sep="/")
##     width <- as.integer(h5read(h5fl, path))
##     checkIdentical(width(ir), width)

##     ## test NAMES
##     ## XXXX FIX ME: what to do??
##     path <- paste(top_name, "NAMES", "data", sep="/")
##     NAMES <- as.name(h5read(h5fl, path))
##     checkIdentical(as.name('\001NULL\001'), NAMES)

##     ## test elementMetadata / mcols
##     ## XXXX FIX ME: what to do?
##     path <- paste(top_name, "elementMetadata", "data", sep="/")
##     elementMetadata <- as.name(h5read(h5fl, path))
##     checkIdentical(as.name('\001NULL\001'), elementType)

##     ## test elementType
##     path <- paste(top_name, "elementType", "data", sep="/")
##     elementType <- as.character(h5read(h5fl, path))
##     checkIdentical(elementType(ir), elementType)

##     ## test metadata?
##     ## XXXX FIX ME: check for absence of dataset called 'metadata'?

##     ## test
    
## }
##test_write_IRanges()

test_encode_bookkeeping_S4 <- function() {
    ir <- IRanges()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    ## create arbitrary H5 object to attach attributes to
    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(ir, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("IRanges", foo_ats[["class"]])
    checkIdentical("IRanges", foo_ats[["package"]])
    checkIdentical("S4SXP", foo_ats[["sexptype"]])
    H5close()
}
##test_encode_bookkeeping_S4()

test_encode_bookkeeping_primitive <- function() {
    ints <- 1:20
    
    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    ## create arbitrary H5 object to attach attributes to
    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(ints, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("integer", foo_ats[["class"]])
    checkIdentical("INTSXP", foo_ats[["sexptype"]])
    H5close()
}
##test_encode_bookkeeping_primitive()

test_encode_bookkeeping_S3 <- function() {
    df <- .data.frame()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    ## create arbitrary H5 object to attach attributes to
    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(df, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("data.frame", foo_ats[["class"]])
    checkIdentical("VECSXP", foo_ats[["sexptype"]])
    H5close()
}

test_encode_bookkeeping_empty_list <- function() {
    l <- list()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    ## create arbitrary H5 object to attach attributes to
    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(l, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("list", foo_ats[["class"]])
    checkIdentical("VECSXP", foo_ats[["sexptype"]])
    H5close()
}

test_encode_bookkeeping_NULLobj <- function() {
    x <- NULL

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(x, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("NULL", foo_ats[["class"]])
    checkIdentical("NILSXP", foo_ats[["sexptype"]])
    H5close()
}
##test_encode_bookkeeping_NULLobj()

## ignore bookkeeping (tested separately)
test_encode_NULLobj <- function() {
    x <- NULL

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(x, h5fl, top_name)
    H5close()
    
    ## adapted from h5readAttributes.R
    loc = rhdf5:::h5checktypeOrOpenLoc(h5fl, readonly=TRUE)
    ## check no data encoded
    checkTrue(!H5Lexists(loc$H5Identifier, "foo/attrs/data"))
    rhdf5:::h5closeitLoc(loc)

    browser()
    foo_attrs <- lapply(h5readAttributes(h5fl, "foo/attrs"), as.character)
    checkIdentical("NULL", foo_attrs[["class"]])
    checkIdentical("NILSXP", foo_attrs[["sexptype"]])
    
}
test_encode_NULLobj()

test_encode_list_with_NULLs <- function() {
    l <- list(x=1:3, y=NULL, z=NULL)

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "list"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    ## test outer (l)
    l_ats <- lapply(h5readAttributes(h5fl, "list"), as.character)
    checkIdentical("list", l_ats[["class"]])
    checkIdentical("VECSXP", l_ats[["sexptype"]])
    H5close()

    ## test x
    x_name <- "list/x"
    x_ats <- lapply(h5readAttributes(h5fl, x_name), as.character)
    checkIdentical("integer", x_ats[["class"]])
    checkIdentical("INTSXP", x_ats[["sexptype"]])
    H5close()
    x_data_name <- paste(x_name, "data", sep="/")
    x_data <- as.integer(h5read(h5fl, x_data_name))
    checkIdentical(l[["x"]], x_data)
    H5close()

    ## test y
    y_name <- "list/y"
    y_ats <- lapply(h5readAttributes(h5fl, y_name), as.character)
    checkIdentical("NULL", y_ats[["class"]])
    checkIdentical("NILSXP", y_ats[["sexptype"]])
    H5close()

    ## test z
    z_name <- "list/z"
    z_ats <- lapply(h5readAttributes(h5fl, z_name), as.character)
    checkIdentical("NULL", z_ats[["class"]])
    checkIdentical("NILSXP", z_ats[["sexptype"]])
    H5close()
}
##test_encode_list_with_NULLs()

test_encode_data.frame <- function() {
    df <- .data.frame()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(df, h5fl, top_name)
    H5close()

    ## outer attributes
    df_ats <- lapply(h5readAttributes(h5fl, top_name), as.character)
    checkIdentical("data.frame", df_ats[["class"]])
    checkIdentical("VECSXP", df_ats[["sexptype"]])
    H5close()
    ## y
    y_name <- "foo/y"
    y_ats <- lapply(h5readAttributes(h5fl, y_name), as.character)
    checkIdentical("integer", y_ats[["class"]])
    checkIdentical("INTSXP", y_ats[["sexptype"]])
    y_data <- as.integer(h5read(h5fl, "foo/y/data"))
    checkIdentical(df[["y"]], y_data)
    H5close()
    ## z
    z_name <- "foo/z"
    z_ats <- lapply(h5readAttributes(h5fl, z_name), as.character)
    H5close()
    checkIdentical("numeric", z_ats[["class"]])
    checkIdentical("REALSXP", z_ats[["sexptype"]])
    z_data <- as.double(h5read(h5fl, "foo/z/data"))
    checkIdentical(df[["z"]], z_data)
    H5close()
}
##test_encode_data.frame()

test_encode_factor <- function() {
    ff <- .factor()
    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(ff, h5fl, top_name)
    H5close()

    ## outer attributes
    foo_ats <- lapply(h5readAttributes(h5fl, top_name), as.character)
    checkIdentical("factor", foo_ats[["class"]])
    checkIdentical("INTSXP", foo_ats[["sexptype"]])
    H5close()

    ## data
    data_name <- paste(top_name, "data", sep="/")
    data <- as.integer(h5read(h5fl, data_name))
    checkIdentical(as.integer(ff), data)

    ## class attribute encoded as group (has its own class attribute also)
    class_group_path <- "foo/class"
    class_ats <- lapply(h5readAttributes(h5fl, class_group_path), as.character)
    checkIdentical("character", class_ats[["class"]])
    checkIdentical("STRSXP", class_ats[["sexptype"]])
    H5close()
    ## value
    data_name <- "foo/class/data"
    data <- as.character(h5read(h5fl, data_name))
    checkIdentical("factor", data)
    H5close()

    ## levels attribute encoded as group
    levels_group_path <- "foo/levels"
    levels_ats <- lapply(h5readAttributes(h5fl, class_group_path), as.character)
    checkIdentical("character", levels_ats[["class"]])
    checkIdentical("STRSXP", levels_ats[["sexptype"]])
    H5close()
    ## value
    data_name <- "foo/levels/data"
    data <- as.character(h5read(h5fl, data_name))
    checkIdentical(levels(ff), data)
}
##test_encode_factor()

test_encode_matrix <- function() {
    mx <- .matrix()
    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(mx, h5fl, top_name)
    H5close()

    ## outer attributes
    foo_ats <- lapply(h5readAttributes(h5fl, top_name), as.character)
    checkIdentical("matrix", foo_ats[["class"]])
    checkIdentical("REALSXP", foo_ats[["sexptype"]])
    H5close()

    ## ## data
    ## data_name <- paste(top_name, "data", sep="/")
    ## data <- as.integer(h5read(h5fl, data_name))
    ## checkIdentical(as.integer(ff), data)

    ## ## class attribute encoded as group (has its own class attribute also)
    ## class_group_path <- "foo/class"
    ## class_ats <- lapply(h5readAttributes(h5fl, class_group_path), as.character)
    ## checkIdentical("character", class_ats[["class"]])
    ## checkIdentical("STRSXP", class_ats[["sexptype"]])
    ## H5close()
    ## ## value
    ## data_name <- "foo/class/data"
    ## data <- as.character(h5read(h5fl, data_name))
    ## checkIdentical("factor", data)
    ## H5close()

    ## ## levels attribute encoded as group
    ## levels_group_path <- "foo/levels"
    ## levels_ats <- lapply(h5readAttributes(h5fl, class_group_path), as.character)
    ## checkIdentical("character", levels_ats[["class"]])
    ## checkIdentical("STRSXP", levels_ats[["sexptype"]])
    ## H5close()
    ## ## value
    ## data_name <- "foo/levels/data"
    ## data <- as.character(h5read(h5fl, data_name))
    ## checkIdentical(levels(ff), data)
}
##test_encode_matrix()
