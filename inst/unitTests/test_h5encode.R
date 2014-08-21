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
    data.frame(x=1, y=1:10, z=seq(1.1, 10.1))
}

.factor <- function() {
    vals <- c("d", "a", "a", "e", "e", "b", "a", "e", "c", "b")
    lvls <- c("a", "b", "c", "d", "e")
    factor(vals, lvls)
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

    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("IRanges", as.character(H5Aread(aid_class)))
    aid_pkg <- H5Aopen(oid, "package")
    checkIdentical("IRanges", as.character(H5Aread(aid_pkg)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("S4SXP", as.character(H5Aread(aid_sexptype)))
    H5close()
}

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

    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("integer", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("INTSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
}

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

    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("data.frame", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("VECSXP", as.character(H5Aread(aid_sexptype)))
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

    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("list", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("VECSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
}

test_encode_NULLobj <- function() {
    x <- NULL

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(x, h5fl, top_name)
    H5close()

    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("NULL", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("NILSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
}

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
    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("list", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("VECSXP", as.character(H5Aread(aid_sexptype)))
    H5close()

    ## test x
    x_name <- paste(top_name, "x", sep="/")
    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, x_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("integer", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("INTSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
    x_data_name <- paste(x_name, "data", sep="/")
    x_data <- as.integer(h5read(h5fl, x_data_name))
    checkIdentical(l[["x"]], x_data)
    H5close()

    ## test y
    y_name <- paste(top_name, "y", sep="/")
    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, y_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("NULL", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("NILSXP", as.character(H5Aread(aid_sexptype)))
    H5close()

    ## test z
    z_name <- paste(top_name, "z", sep="/")
    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, z_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("NULL", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("NILSXP", as.character(H5Aread(aid_sexptype)))
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

    browser()
}
test_encode_data.frame()

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
    fid <- H5Fopen(h5fl)
    oid <- H5Oopen(fid, top_name)
    aid_class <- H5Aopen(oid, "class")
    checkIdentical("factor", as.character(H5Aread(aid_class)))
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("INTSXP", as.character(H5Aread(aid_sexptype)))
    H5close()

    ## data
    data_name <- paste(top_name, "data", sep="/")
    data <- as.integer(h5read(h5fl, data_name))
    checkIdentical(as.integer(ff), data)

    ## class attribute encoded as group (has its own class attribute also)
    class_group_path <- paste(top_name, "class", sep="/")
    fid <- H5Fopen(h5fl)
    gid <- H5Gopen(fid, class_group_path)
    ## class attribute (of class group)
    aid_class <- H5Aopen(gid, "class")
    checkIdentical("character", as.character(H5Aread(aid_class)))
    ## sexptype
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("STRSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
    ## value
    data_name <- paste(class_group_path, "data", sep="/")
    data <- as.character(h5read(h5fl, data_name))
    checkIdentical("factor", data)
    H5close()

    ## levels attribute encoded as group
    levels_group_path <- paste(top_name, "levels", sep="/")
    fid <- H5Fopen(h5fl)
    gid <- H5Gopen(fid, levels_group_path)
    ## class attribute (of levels group)
    aid_class <- H5Aopen(gid, "class")
    checkIdentical("character", as.character(H5Aread(aid_class)))
    ## sexptype
    aid_sexptype <- H5Aopen(oid, "sexptype")
    checkIdentical("STRSXP", as.character(H5Aread(aid_sexptype)))
    H5close()
    ## value
    data_name <- paste(levels_group_path, "data", sep="/")
    data <- as.character(h5read(h5fl, data_name))
    checkIdentical(levels(ff), data)
}
##test_encode_factor()
