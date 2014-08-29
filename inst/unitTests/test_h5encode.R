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

## only expected to work for datasets and groups
.isAbsent <- function(file, name) {
    loc = rhdf5:::h5checktypeOrOpenLoc(file, readonly=TRUE)
    res <- !H5Lexists(loc$H5Identifier, name)
    rhdf5:::h5closeitLoc(loc)
    res
}

## trace(rhdf5:::encode, quote(print(obj)))
## trace(rhdf5:::encode_bookkeeping, quote(print(obj)))
## trace(rhdf5:::encode_list_like, quote(print(obj)))
## trace(rhdf5:::encode.default, browser)
## trace(rhdf5:::encode_attrs, browser)
## trace(rhdf5:::encode_data, browser)
options(error=recover)

test_write_IRanges <- function() {
    ir <- .simpleIRanges()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"
    
    rhdf5:::encode(ir, h5fl, top_name)
    H5close()

    ## ensure no data recorded (since S4)
    checkTrue(.isAbsent(h5fl, "foo/data"))

    attrs <- attributes(ir)

    ## attrs:NAMES
    NAMES_name <- "foo/attrs/NAMES/data/data"
    NAMES_data <- as.character(h5read(h5fl, NAMES_name))
    checkIdentical(as.character(attrs[["NAMES"]]), NAMES_data)

    ## attrs:class:data
    class_data_name <- "foo/attrs/class/data/data"
    class_data_data <- as.character(h5read(h5fl, class_data_name))
    checkIdentical(as.character(attrs[["class"]]), class_data_data)

    ## attrs:class:attrs:package:data
    class_package_name <- "foo/attrs/class/attrs/package/data/data"
    class_package_data <- as.character(h5read(h5fl, class_package_name))
    class_attr <- attrs[["class"]]
    pkg_attr <- attr(class_attr, "package")
    checkIdentical(pkg_attr, class_package_data)

    ## attrs:elementMetadata
    elMdata_name <- "foo/attrs/elementMetadata/data/data"
    elMdata_data <- as.character(h5read(h5fl, elMdata_name))
    checkIdentical(as.character(attrs[["elementMetadata"]]), elMdata_data)

    ## attrs:elementType
    elType_name <- "foo/attrs/elementType/data/data"
    elType_data <- as.character(h5read(h5fl, elType_name))
    checkIdentical(as.character(attrs[["elementType"]]), elType_data)

    ## attrs:metadata
    ## FIX ME: questionable; since zero-length list, encoded only bookkeping
    metadata_name <- "foo/attrs/metadata"
    metadata_ats <- lapply(h5readAttributes(h5fl, metadata_name), as.character)
    checkIdentical("list", metadata_ats[["class"]])
    checkIdentical("VECSXP", metadata_ats[["sexptype"]])
    checkTrue(.isAbsent(h5fl, "foo/attrs/metadata/data"))
    checkTrue(.isAbsent(h5fl, "foo/attrs/metadata/attrs"))

    ## attrs:start
    start_name <- "foo/attrs/start/data/data"
    start_data <- as.integer(h5read(h5fl, start_name))
    checkIdentical(attrs[["start"]], start_data)

    ## attrs:width
    width_name <- "foo/attrs/width/data/data"
    width_data <- as.integer(h5read(h5fl, width_name))
    checkIdentical(attrs[["width"]], width_data)
}
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

test_encode_bookkeeping_primitive <- function() {
    ints <- 1:3
    
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

test_encode_primitive <- function() {
    x <- 1:3

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(x, h5fl, top_name)
    H5close()

    checkTrue(.isAbsent(h5fl, "foo/attrs"))

    data_name <- "foo/data/data"
    data <- as.integer(h5read(h5fl, data_name))
    checkIdentical(x, data)
}
##test_encode_primitive()

test_encode_bookkeeping_SYMSXP <- function() {
    n <- as.name('\001NULL\001')

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    h5createGroup(h5fl, top_name)
    rhdf5:::encode_bookkeeping(n, h5fl, top_name)
    H5close()

    foo_ats <- lapply(h5readAttributes(h5fl, "foo"), as.character)
    checkIdentical("name", foo_ats[["class"]])
    checkIdentical("SYMSXP", foo_ats[["sexptype"]])
    H5close()
}
##test_encode_bookkeeping_SYMSXP()

test_encode_SYMSXP <- function() {
    n <- as.name('\001NULL\001')

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(n, h5fl, top_name)
    H5close()

    checkTrue(.isAbsent(h5fl, "foo/attrs"))
    data_name <- "foo/data/data"
    data <- as.character(h5read(h5fl, data_name))
    checkIdentical(as.character(n), data)
}
##test_encode_SYMSXP()

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

test_encode_NULLobj <- function() {
    x <- NULL

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(x, h5fl, top_name)
    H5close()
    
    checkTrue(.isAbsent(h5fl, "foo/attrs"))
    checkTrue(.isAbsent(h5fl, "foo/data"))
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
##test_encode_bookkeeping_empty_list()

test_encode_empty_list <- function() {
    l <- list()

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    checkTrue(.isAbsent(h5fl, "foo/attrs"))
    checkTrue(.isAbsent(h5fl, "foo/data"))
}
##test_encode_empty_list()

test_encode_list_unnamed <- function() {
    l <- list(1:3, letters[5:7])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    elt1_data_name <- "foo/data/elt1/data/data"
    elt1_data <- as.integer(h5read(h5fl, elt1_data_name))
    checkIdentical(l[[1]], elt1_data)
    checkTrue(.isAbsent(h5fl, "foo/data/elt1/attrs"))

    elt2_data_name <- "foo/data/elt2/data/data"
    elt2_data <- as.character(h5read(h5fl, elt2_data_name))
    checkIdentical(l[[2]], elt2_data)
    checkTrue(.isAbsent(h5fl, "foo/data/elt2/attrs"))

    ## absence of attributes
    checkTrue(.isAbsent(h5fl, "foo/attrs"))
}
##test_encode_list_unnamed()

test_encode_list_named <- function() {
    l <- list(y=c("x", ""), z=letters[5:7])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    elt1_data_name <- "foo/data/elt1/data/data"
    elt1_data <- as.character(h5read(h5fl, elt1_data_name))
    checkIdentical(l[[1]], elt1_data)

    elt2_data_name <- "foo/data/elt2/data/data"
    elt2_data <- as.character(h5read(h5fl, elt2_data_name))
    checkIdentical(l[[2]], elt2_data)

    names_name <- "foo/attrs/names/data/data"
    names_data <- as.character(h5read(h5fl, names_name))
    checkIdentical(names(l), names_data)
}
##test_encode_list_named()

test_encode_list_partially_named <- function() {
    l <- list(y=c("x", ""), letters[1:3])

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    elt1_data_name <- "foo/data/elt1/data/data"
    elt1_data <- as.character(h5read(h5fl, elt1_data_name))
    checkIdentical(l[[1]], elt1_data)

    elt2_data_name <- "foo/data/elt2/data/data"
    elt2_data <- as.character(h5read(h5fl, elt2_data_name))
    checkIdentical(l[[2]], elt2_data)

    names_name <- "foo/attrs/names/data/data"
    names_data <- as.character(h5read(h5fl, names_name))
    checkIdentical(names(l), names_data)
}
##test_encode_list_partially_named()

test_encode_list_with_NULLs <- function() {
    l <- list(x=1:3, y=NULL, NULL)

    h5fl <- tempfile(fileext=".h5")
    if(interactive())
        message(h5fl)
    h5createFile(h5fl)
    top_name <- "foo"

    rhdf5:::encode(l, h5fl, top_name)
    H5close()

    ## test attrs
    names_name <- "foo/attrs/names/data/data"
    names_data <- as.character(h5read(h5fl, names_name))
    checkIdentical(names(l), names_data)
    H5close()

    ## test x
    x_name <- "foo/data/elt1/data/data"
    x_data <- as.integer(h5read(h5fl, x_name))
    checkIdentical(l[["x"]], x_data)
    H5close()

    ## test y
    y_name <- "foo/data/elt2"
    y_ats <- lapply(h5readAttributes(h5fl, y_name), as.character)
    checkIdentical("NULL", y_ats[["class"]])
    checkIdentical("NILSXP", y_ats[["sexptype"]])
    H5close()

    ## test z
    z_name <- "foo/data/elt3"
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

    ## attrs
    class_name <- "foo/attrs/class/data/data"
    class_data <- as.character(h5read(h5fl, class_name))
    checkIdentical(class(df), class_data)

    names_name <- "foo/attrs/names/data/data"
    names_data <- as.character(h5read(h5fl, names_name))
    checkIdentical(names(df), names_data)

    row.names_name <- "foo/attrs/row.names/data/data"
    row.names_data <- as.character(h5read(h5fl, row.names_name))
    checkIdentical(row.names(df), row.names_data)
    
    ## y
    y_name <- "foo/data/elt1/data/data"
    y_data <- as.integer(h5read(h5fl, y_name))
    checkIdentical(df[["y"]], y_data)
    H5close()

    ## z
    z_name <- "foo/data/elt2/data/data"
    z_data <- as.double(h5read(h5fl, z_name))
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
    data_name <- "foo/data/data"
    data <- as.integer(h5read(h5fl, data_name))
    checkIdentical(as.integer(ff), data)

    ## attrs
    class_name <- "foo/attrs/class/data/data"
    class_data <- as.character(h5read(h5fl, class_name))
    checkIdentical("factor", class_data)
    H5close()

    levels_name <- "foo/attrs/levels/data/data"
    levels_data <- as.character(h5read(h5fl, levels_name))
    checkIdentical(levels(ff), levels_data)
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

    ## data
    data_name <- "foo/data/data"
    data_data <- as.integer(h5read(h5fl, data_name))
    checkIdentical(as.integer(mx), data_data)

    ## dim
    dim_name <- "foo/attrs/dim/data/data"
    dim_data <- as.integer(h5read(h5fl, dim_name))
    checkIdentical(dim(mx), dim_data)

    ## dimnames:data
    ##   a
    dimnames_data_name <- "foo/attrs/dimnames/data/elt1/data/data"
    dimnames_data <- as.character(h5read(h5fl, dimnames_data_name))
    checkIdentical(dimnames(mx)[[1]], dimnames_data)
    ##   b
    dimnames_data_name <- "foo/attrs/dimnames/data/elt2/data/data"
    dimnames_data <- as.character(h5read(h5fl, dimnames_data_name))
    checkIdentical(dimnames(mx)[[2]], dimnames_data)

    ## dimnames::attrs
    dimnames_names_name <- "foo/attrs/dimnames/attrs/names/data/data"
    dimnames_names_data <- as.character(h5read(h5fl, dimnames_names_name))
    checkIdentical(names(dimnames(mx)), dimnames_names_data)
}
##test_encode_matrix()
