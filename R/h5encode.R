##h5write <- function(...) print(...)

h5type <- function(x) {
    switch(typeof(x), logical="LGLSXP", integer="INTSXP",
           double="REALXSP", S4="S4SXP", list="VECSXP",
           stop("unhandled type ", typeof(x)))
}

encode <- function(obj, file, name, ...)
    UseMethod("encode")

setGeneric("encode")

setMethod("encode", "ANY", function(obj, file, name, ...) {
    if (isS4(obj)) {
        message("S4!")
        browser()
        h5createGroup(file, name)
        ## recursively encode each attribute
        encode_attributes(attributes(obj), file, name, ...)
    } else {
        message("not S4!")
        browser()
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

encode_attributes <- function(attrs, file, name, ...) {
    ## do nothing for NULL attributes
    if(is.null(attrs))
        return()
    message("encode_attributes!")
    browser()
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
    browser()
    encode_attributes(attributes(obj), file, name, ...)
    attributes(obj) <- NULL
    class(obj) <- "name"
    h5write(as.character(obj), file, paste(name, "name", sep="/"),
            write.attributes=TRUE)
}

encode.default <- function(obj, file, name, ...) {
    message("encode.default!")
    browser()
    data_name <- paste(name, "data", sep="/")
    encode_attributes(attributes(obj), file, name, ...)
    h5write(obj, file, data_name, ...)
}

## setMethod("encode", "integer", function(obj, file, name, ...) {
##     encode_primitive(obj, file, name, ...)
## })
## setMethod("encode", "double", function(obj, file, name, ...) {
##     encode_primitive(obj, file, name, ...)
## })
## setMethod("encode", "logical", function(obj, file, name, ...) {
##     encode_primitive(obj, file, name, ...)
## })
## setMethod("encode", "character", function(obj, file, name, ...) {
##     encode_primitive(obj, file, name, ...)
## })

## setMethod("encode", "matrix", function(obj, ...) {
    
## })

## encode.Foo3 <- function(obj, ...) "Foo3"
