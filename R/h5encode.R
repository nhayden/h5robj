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
        attrs <- attributes(obj)
        ## recursively encode each attribute
        for (attr_name in names(attrs)) {
            val <- attrs[[attr_name]]
            ## only encode if meaningful value
            if(!is.null(val) && length(val) != 0L) {
                sub_name <- paste(name, attr_name, sep="/")
                encode(attrs[[attr_name]], file, sub_name, ...)
            }
        }
    } else {
        message("not S4!")
        browser()
        if(class(obj) == "name") {
            message("ignoring NAMES attribute")
            return()
        }
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

encode.default <- function(obj, file, name, ...) {
    message("encode.default!")
    browser()
    data_name <- paste(name, "data", sep="/")
    attrs <- attributes(obj)
    if(!is.null(attrs)) {
        ## recursively encode each attribute
        for (attr_name in names(attrs)) {
            val <- attrs[[attr_name]]
            ## only encode if meaningful value
            if(!is.null(val) && length(val) != 0L) {
                sub_name <- paste(name, attr_name, sep="/")
                encode(attrs[[attr_name]], file, sub_name, ...)
            }
        }
    }
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
