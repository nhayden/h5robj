##h5write <- function(...) print(...)

h5type <- function(x) {
    switch(typeof(x),
           ## primitives
           logical="LGLSXP", integer="INTSXP", double="REALXSP",
           raw="RAWSXP",
           ## other
           S4="S4SXP", list="VECSXP", NULL="NILSXP", name="SYMSXP",
           stop("unhandled type '", typeof(x), "'"))
}

encode <- function(obj, file, name, ...)
    UseMethod("encode")

setGeneric("encode")

setMethod("encode", "ANY", function(obj, file, name, ...) {
    if (isS4(obj)) {
        message("S4!")
        ##browser()
        h5createGroup(file, name)
        ## recursively encode each attribute
        encode_attributes(attributes(obj), file, name, ...)
    } else {
        message("not S4!")
        ##browser()
        h5createGroup(file, name)
        if ("class" %in% names(attributes(obj))) {
            stop("S3 not implemented")
            message("S3!")
            ##h5write("S3")
            callNextMethod(obj, file, name, ...)
        } else {
            callNextMethod(obj, file, name, ...)
            ##h5write(sprintf("unimplemented '%s'", class(obj)))
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

encode_attributes <- function(attrs, file, name, ...) {
    ## do nothing for NULL attributes
    if(is.null(attrs))
        return()
    message("encode_attributes!")
    ##browser()
    if( !is.list(attrs) )
        stop("attrs must be list, got ", class(attrs))
    if( !all(names(attrs) != ""))
        stop("attrs must be named list")
    ## recursively encode each attribute
    for(attr_name in names(attrs)) {
        val <- attrs[[attr_name]]
        ## encode only meaningful values
        if(!is.null(val) && length(val) != 0L) {
            sub_name <- paste(name, attr_name, sep="/")
            encode(attrs[[attr_name]], file, sub_name, ...)
        }
    }
}

encode.name <- function(obj, file, name, ...) {
    message("encode.name!")
    ##browser()
    encode_attributes(attributes(obj), file, name, ...)
    h5write(as.character(obj), file, paste(name, "data", sep="/"),
            write.attributes=TRUE)
}

encode.default <- function(obj, file, name, ...) {
    message("encode.default!")
    ##browser()
    data_name <- paste(name, "data", sep="/")
    encode_attributes(attributes(obj), file, name, ...)
    h5write(obj, file, data_name, ...)
}

## setMethod("encode", "matrix", function(obj, ...) {
    
## })

## encode.Foo3 <- function(obj, ...) "Foo3"
