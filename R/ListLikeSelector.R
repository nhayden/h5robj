.h5id <- setClass("h5id", slots=list(h5file="character", h5root="character"))
setGeneric("h5file", function(x) standardGeneric("h5file"))
setGeneric("h5root", function(x) standardGeneric("h5root"))

setMethod("h5file", "h5id", function(x) x@h5file)
setMethod("h5root", "h5id", function(x) x@h5root)
setMethod(show, "h5id", function(object) {
    cat("class: ", class(object), "\n")
    values <- sapply(slotNames(object), slot, object=object)
    ## info <- paste(slotNames(object), values, sep=": ", collapse="; ")
    ## cat(strwrap(info, exdent=2), sep="\n")
    info <- paste(slotNames(object), values, sep=": ")
    cat(strwrap(info, exdent=2, indent=2), sep="\n")
})

h5id <- function(file, root) {
    if(!file.exists(file))
        stop("file does not exist: '", file, "'")
    if(!h5exists(file, root))
        stop("path '", root, "' does not exist in file '", file, "'")
    .h5id(h5file=file, h5root=root)
}

## In terms of h5robj encode storage, 'root' slot of this class is
## one-up from a materializable object. E.g., in
## 'foo/attrs/names/data/data', a Selector might be at
## 'foo/attrs/names', whereas a ListLikeSelector would be at
## 'foo/attrs'. That is, each immediate descendant of the
## ListLikeSelector is a materializable object.
.ListLikeSelector <- setClass("ListLikeSelector",
                              slots=list(
                                ## h5data="list", ## nested Selectors
                                ## h5attrs="list")) ## nested Selectors
                                selectors="list"))
setGeneric("h5ids", function(x) setGeneric("h5ids"))
setMethod("h5ids", "ListLikeSelector", function(x) {
    ids <- if(length(x@selectors) > 0L)
        lapply(x@selectors, function(selector) {
            if(is(selector, "Implicit"))
                "Implicit"
            else
                selector@h5identifier
        })
    else
        list()
    ##browser()
    ids
})
setMethod(show, "ListLikeSelector", function(object) {
    cat("class: ", class(object), "\n")
    if(length(object@selectors) == 0L) {
        cat("selectors: list()", "\n")
    } else {
        print(h5ids(object))
    }
})

implicit_by_default_attrs <- c("names", "row.names", "class", "levels", "dim")

## second param is group_path instead of typical 'root' because, per
## the description with the LLS def, it's not the root of an object,
## but a sub-GROUP, e.g., 'data' or 'attrs'
ListLikeSelector <- function(file, group_path, is.attrs=FALSE,
                             all.implicit=FALSE, selectors=NULL) {
    ## for creating a ListLikeSelector directly, e.g., when converting
    ## Implicits
    if(!is.null(selectors))
        return(.ListLikeSelector(selectors=selectors))
    if(!h5exists(file, group_path))
        return(.ListLikeSelector(selectors=list()))
    descs_paths <- h5ls_immediate_descendants(file, group_path)
    descs_names <- basename(descs_paths)
    sels <- NULL
    if(all.implicit) {
        sels <- lapply(descs_paths, function(x)
                       Implicit(file, x))
    } else {
        sels <- lapply(descs_paths, function(x) {
            ## add more to right operand of %in% as they become relevant
            if(is.attrs && basename(x) %in% implicit_by_default_attrs)
                Implicit(file, x)
            else
                Selector(file, x)
        })
    }
    if(is.attrs)
        names(sels) <- descs_names
    .ListLikeSelector(selectors=sels)
}
