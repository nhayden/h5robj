encode_bookkeeping <- function(obj, file, name, ...) {
    fid <- H5Fopen(file)
    oid <- H5Oopen(fid, name)
    if(isS4(obj)) {
        if(is.null(attr(class(obj), "package")))
            stop("cannot encode: package attribute required on",
                 " S4 object's class attribute")
        h5writeAttribute(attr(class(obj), "package"), oid, "package")
    }
    h5writeAttribute(as.character(class(obj)), oid, "class")
    h5writeAttribute(h5type(obj), oid, "sexptype")
    H5close()
}

encode <- function(obj, file, name, ...)
    UseMethod("encode")

setGeneric("encode", signature="obj")

setMethod("encode", "ANY", function(obj, file, name, ...) {
    h5createGroup(file, name)
    encode_bookkeeping(obj, file, name)
    encode_attrs(obj, file, name)
    ## 0-length objs should only encode bookkeeping and attrs
    if(is.null(obj) || length(obj) == 0L)
        return()
    encode_data(obj, file, name)
})

encode_attrs <- function(obj, file, name, ...) {
    if(is.null(obj) || is.null(attributes(obj)))
        return()
    attrs_name <- paste(name, "attrs", sep="/")
    encode_list_like(attributes(obj), file, attrs_name,
                     must.use.names=TRUE, ...)
}

encode_data <- function(obj, file, name, ...) {
    if(is.null(obj))
        return()
    data_name <- paste(name, "data", sep="/")
    ## Is there a better way to decompose attributes and data on S4?
    attributes(obj) <- NULL
    if(is.recursive(obj)) {
        encode_list_like(obj, file, data_name, ...)
    } else {
        callNextMethod(obj, file, data_name, ...)
    }
}

encode_list_like <- function(obj, file, name, must.use.names=FALSE, ...) {
    if( !is.list(obj) || length(obj) == 0L )
        stop("'obj' must be a non-zero length list, got '", class(obj), "'")
    if(must.use.names && is.null(names(obj)))
        stop("must.use.names TRUE, but no names attribute")
    h5createGroup(file, name)

    ## recursively encode each attribute in separate GROUP
    group_names <- list()
    if(must.use.names)
        group_names <- names(obj)
    else
        group_names <- paste0("elt", seq_along(obj))
    for(i in seq_along(obj)) {
        sub_name <- paste(name, group_names[[i]], sep="/")
        encode(obj[[i]], file, sub_name, ...)
    }
}

encode.name <- function(obj, file, name, ...) {
    if(is.null(obj))
        return()
    h5createGroup(file, name)
    data_name <- paste(name, "data", sep="/")
    h5write(as.character(obj), file, data_name, ...)
}

encode.default <- function(obj, file, name, ...) {
    ## FIX ME: is there a way to avoid knowing about "S4" class (which
    ## represents an empty obj with the S4 bit set)?
    if(is.null(obj) || class(obj) == "S4")
        return()
    h5createGroup(file, name)
    data_name <- paste(name, "data", sep="/")
    h5write(obj, file, data_name, ...)
}
