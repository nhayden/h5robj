h5type <- function(x) {
    switch(typeof(x),
           ## primitives
           logical="LGLSXP", integer="INTSXP", double="REALSXP",
           character="STRSXP", raw="RAWSXP",
           ## other
           S4="S4SXP", list="VECSXP", NULL="NILSXP", symbol="SYMSXP",
           stop("unhandled type '", typeof(x), "'"))
}

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

setGeneric("encode")

setMethod("encode", "ANY", function(obj, file, name, ...) {
    h5createGroup(file, name)
    encode_bookkeeping(obj, file, name)
    if(is.null(obj) || length(obj) == 0L) ## check length for empty lists
        return()
    encode_attrs(obj, file, name)
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
    ## XXXX FIX ME: how to neatly decompose data and attrs?
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
    ## XXXX FIX ME: shouldn't have to know about "S4" class, which
    ## represents an empty obj with the S4 bit set
    if(is.null(obj) || class(obj) == "S4")
        return()
    h5createGroup(file, name)
    data_name <- paste(name, "data", sep="/")
    h5write(obj, file, data_name, ...)
}
