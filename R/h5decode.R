decode_bookkeeping <- function(file, name, ...) {
    bookkeeping <- lapply(h5readAttributes(file, name), as.character)
    H5close()
    bookkeeping
}

decode <- function(file, name, ...) {
    bkk <- decode_bookkeeping(file, name, ...)
    bkk_names <- names(bkk)
    if( !("class" %in% bkk_names) || !("sexptype" %in% bkk_names) )
        stop("Can't decode; missing bookkeeping information")
    if( bkk[["sexptype"]] == "S4SXP" && !("package" %in% bkk_names) )
        stop("Can't decode; S4 object without package information")

    if(bkk[["sexptype"]] == "S4SXP")
        decode_S4(file, name, bkk)
    else
        decode_S3(file, name, bkk)
}

decode_S3 <- function(file, name, bookkeeping) {
    cl_name <- bookkeeping[["class"]]
    if(cl_name == "factor") {
        proto_obj <- structure(integer(), cl_name)
    } else {
        proto_obj <- structure(list(), cl_name)
    }
    .decode(proto_obj, file, name)
}

.decode <- function(obj, file, name, ...) {
    UseMethod(".decode")
}

.decode.default <- function(obj, file, name, ...) {
    message("decode.default!")

    attrs <- decode_atttrs(file, name)
    data <- decode_data(file, name)

    attributes(data) <- attrs
    data
}

decode_S4 <- function(file, name, bookkeeping)
{
    pkg <- bookkeeping[["package"]]
    nmspc <- loadNamespace(pkg) ## changed from if(!requireNamespace())
    class_name <- bookkeeping[["class"]]
    class_def <- getClass(class_name, where=nmspc)
    if (!identical(pkg, attr(class_def@className, "package")))
        stop("class '", class_name, "' not defined in package '", pkg, "'")

    ## read attributes other than 'class'
    attrs <- decode_attrs(file, name) ## list
    
    ## initialize with initialize,ANY-method
    initANY <- getMethod(initialize, "ANY")
    proto_obj <- .Call(methods:::C_new_object, class_def)
    do.call(initANY, c(proto_obj, attrs))
}
