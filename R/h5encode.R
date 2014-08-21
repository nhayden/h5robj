h5type <- function(x) {
    switch(typeof(x),
           ## primitives
           logical="LGLSXP", integer="INTSXP", double="REALSXP",
           character="STRSXP", raw="RAWSXP",
           ## other
           S4="S4SXP", list="VECSXP", NULL="NILSXP", name="SYMSXP",
           stop("unhandled type '", typeof(x), "'"))
}

encode <- function(obj, file, name, ...)
    UseMethod("encode")

setGeneric("encode")

setMethod("encode", "ANY", function(obj, file, name, ...) {
    h5createGroup(file, name)
    ##browser()
    encode_bookkeeping(obj, file, name)
    ## if NULL, only need to record bookkeeping (namely "NILSXP")
    if(!is.null(obj)) {
        if (isS4(obj)) {
            message("S4!")
            ##browser()
            ## recursively encode each attribute
            encode_list_like(attributes(obj), file, name, ...)
        } else {
            message("not S4!")
            ##browser()
            if(is.recursive(obj)) {
                encode_list_like(obj, file, name, ...)
            } else {
                callNextMethod(obj, file, name, ...)
            }
        }
    }
})

encode_bookkeeping <- function(obj, file, name, ...) {
    message("encode_bookkeeping!")
    fid <- H5Fopen(file)
    oid <- H5Oopen(fid, name)
    ## XXXX FIX ME: encode package as NULL in non-S4 cases?
    if(isS4(obj)) {
        if(is.null(attr(class(obj), "package")))
            stop("cannot encode: package attribute required on S4 object's class attribute")
        h5writeAttribute(attr(class(obj), "package"), oid, "package")
    }
    h5writeAttribute(as.character(class(obj)), oid, "class")
    h5writeAttribute(h5type(obj), oid, "sexptype")

    H5close()
}

encode_list_like <- function(obj, file, name, ...) {
    ## do nothing for NULL objects (that is, the "data" part;
    ## bookkeeping encoded in separate function)
    if(is.null(obj))
        return()
    message("encode_list_like!")
    ##browser()
    if( !is.list(obj) )
        stop("obj must be list, got '", class(obj), "'")
    if( !all(names(obj) != ""))
        stop("obj must be named list")
    ## recursively encode each attribute
    for(attr_name in names(obj)) {
        val <- obj[[attr_name]]
        sub_name <- paste(name, attr_name, sep="/")
        encode(obj[[attr_name]], file, sub_name, ...)
    }
}

encode.name <- function(obj, file, name, ...) {
    message("encode.name!")
    ##browser()
    encode_list_like(attributes(obj), file, name, ...)
    h5write(as.character(obj), file, paste(name, "data", sep="/"),
            write.attributes=TRUE)
}

encode.default <- function(obj, file, name, ...) {
    message("encode.default!")
    ##browser()
    encode_list_like(attributes(obj), file, name, ...)
    data_name <- paste(name, "data", sep="/")
    ## expose raw "nugget"
    attributes(obj) <- NULL
    h5write(obj, file, data_name, ...)
}
