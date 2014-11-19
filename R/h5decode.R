decode_bookkeeping <- function(id) {
    ##bookkeeping <- lapply(h5readAttributes(sel@file, sel@root), as.character)
    bookkeeping <- lapply(h5readAttributes(h5file(id), h5root(id)), as.character)
    H5close()
    bookkeeping
}

decode <- function(file, name, ...) {
    decodeSel(AllS(file, name))
}

decodeSel <- function(sel) {
    bkk <- decode_bookkeeping(sel@h5identifier)
    bkk_names <- names(bkk)
    if( !("class" %in% bkk_names) || !("sexptype" %in% bkk_names) )
        stop("Can't decode; missing bookkeeping information in file")
    if( bkk[["sexptype"]] == "S4SXP" && !("package" %in% bkk_names) )
        stop("Can't decode; S4 object without package information")

    if("package" %in% bkk_names)
        decode_S4(sel, bkk)
    else
        decode_S3(sel, bkk)
}

decode_S3 <- function(sel, bookkeeping) {
    cl_name <- bookkeeping[["class"]]
    nugget_sexptype <- Rtype(bookkeeping[["sexptype"]])
    if( !(nugget_sexptype %in% .classless_types) ) {
        stop("decoding S3 for type '", cl_name, "' not implemented")
    }
    proto_obj <- NULL
    if(cl_name == "NULL") {
        return(proto_obj)
    } else if(cl_name == "name") { ## 'name' / 'symbol'
        ## R's pseudo NULL object
        proto_obj <- as.name('\001NULL\001')
    } else if(nugget_sexptype %in% .classless_types) {
        proto_obj <- vector(nugget_sexptype)
    } else {
        proto_obj <- structure(vector(nugget_sexptype), class=cl_name)
    }
    .decode(proto_obj, sel, bookkeeping)
}

.decode <- function(obj, sel, bookkeeping, ...) {
    UseMethod(".decode")
}

decode_list_like <- function(llsel, retain.names=FALSE, ...) {
    res <- llsel@selectors
    if(!retain.names) {
        ## FIX ME: include check that names attribute exists in H5?
        names(res) <- NULL
    }
    for(i in seq_along(res)) {
        ##res[[i]] <- decode(sel@file, list_elts[[i]], ...)
        res[[i]] <- decodeSel(res[[i]])
    }
    res
}

setGeneric("read_nugget", function(sel, ...) standardGeneric("read_nugget"))
setMethod("read_nugget", "AtomicSelector",
    function(sel, ...) {
        h5ident <- sel@h5identifier
        if(!h5exists(h5file(sel@h5identifier), sel@mapper))
            stop("data nugget for '", sel@root, "' does not exist in file")
        if(sum(sel@dimSelection[[1L]]) > 0L) {
            idx <- as.which(sel@dimSelection[[1L]])
            as.vector(h5read(h5file(h5ident), sel@mapper, index=list(idx)))
        } else {
            error("0 selections not yet supported")
            ## likely course of action: use code from decode_S3 that
            ## creates empty S3 object
        }
    }
)
setMethod("read_nugget", "Implicit",
    function(sel, ...) {
        h5ident <- sel@h5identifier
        data_path <- paste(h5root(h5ident), "data/data", sep="/")
        if(!h5exists(h5file(sel@h5identifier), data_path))
            stop("data nugget for '", sel@root, "' does not exist in file")
        as.vector(h5read(h5file(h5ident), data_path))
    }
)

setGeneric("decode_data", function(sel) standardGeneric("decode_data"))

setMethod("decode_data", "Implicit", function(sel) read_nugget(sel))
setMethod("decode_data", "AtomicSelector", function(sel)
          read_nugget(sel))
setMethod("decode_data", "RecursiveSelector", function(sel)
          decode_list_like(sel@h5data))
## XXXX FIX ME: Make specialization that uses a single dimSelection
## object repeatedly so it's not copied for each data member
setMethod("decode_data", "RectSelector",
    function(sel) {
        row_selection <- sel@row_selection
        dimSelection <- list(as.bit.which(row_selection, max(row_selection)))
        llsel_selectors <- sel@h5data@selectors
        for(elt in seq_along(llsel_selectors)) {
            if(is(llsel_selectors[[elt]], "Implicit")) {
                h5ident <- llsel_selectors[[elt]]@h5identifier
                datasel <- AtomicSelector(h5file(h5ident),
                                          h5root(h5ident))
                datasel@dimSelection <- dimSelection
                llsel_selectors[[elt]] <- datasel
            }
        }
        ##browser()
        llsel <- ListLikeSelector(selectors=llsel_selectors)
        decode_list_like(llsel, retain.names=FALSE)
    }
)

decode_attrs <- function(sel, bookkeeping, ...) {
    if(is(sel, "Implicit") || length(sel@h5attrs@selectors) == 0L) {
        NULL
    } else {
        llsel_selectors <- sel@h5attrs@selectors
        ## fill Implicits with top-level selections
        if(is(sel, "AtomicSelector")) {
            to_convert <- which( ! (names(llsel_selectors) %in% c("class",
                                                                  "levels")) )
            for(elt in to_convert) {
                if(is(llsel_selectors[[elt]], "Implicit")) {
                    h5ident <- llsel_selectors[[elt]]@h5identifier
                    attrsel <- AtomicSelector(h5file(h5ident),
                                              h5root(h5ident))
                    attrsel@dimSelection <- sel@dimSelection
                    llsel_selectors[[elt]] <- attrsel
                }
            }
        } else if(is(sel, "RecursiveSelector")) {
            attr_names <- names(llsel_selectors)
            ## process names specially
            if("names" %in% attr_names) {
                names_attr <- llsel_selectors[["names"]]
                if(is(names_attr, "Implicit")) {
                    h5ident <- names_attr@h5identifier
                    new_names_attr <- AtomicSelector(h5file(h5ident),
                                                     h5root(h5ident))
                    selection_indices <- sel@selection_indices
                    top_level_selection <- as.bit.which(selection_indices,
                                                        max(selection_indices))
                    new_names_attr@dimSelection <- list(top_level_selection)
                    llsel_selectors[["names"]] <- new_names_attr
                } else {
                    ## using an explicit names selector; must check
                    ## lengths coincide
                    data_length <- length(sel@h5data@selectors)
                    names_length <- sum(names_attr@dimSelection[[1]])
                    if(data_length != names_length) {
                        stop("length of selected names must match number ",
                             "of data elements in RecursiveSelector")
                    }
                }
            }
        } else if(is(sel, "RectSelector")) {
            attr_names <- names(llsel_selectors)
            if("names" %in% attr_names) {
                names_attr <- llsel_selectors[["names"]]
                if(is(names_attr, "Implicit")) {
                    h5ident <- names_attr@h5identifier
                    new_names_attr <- AtomicSelector(h5file(h5ident),
                                                     h5root(h5ident))
                    selection_indices <- sel@col_selection
                    col_selection <- as.bit.which(selection_indices,
                                                  max(selection_indices))
                    new_names_attr@dimSelection <- list(col_selection)
                    llsel_selectors[["names"]] <- new_names_attr
                } else {
                    stop("Using non-Implicit selectors for names not ",
                         "supported for class 'RectSelector'")
                }
            }
            if("row.names" %in% attr_names) {
                row.names_attr <- llsel_selectors[["row.names"]]
                if(is(row.names_attr, "Implicit")) {
                    h5ident <- row.names_attr@h5identifier
                    new_row.names_attr <- AtomicSelector(h5file(h5ident),
                                                         h5root(h5ident))
                    selection_indices <- sel@row_selection
                    row_selection <- as.bit.which(selection_indices,
                                                  max(selection_indices))
                    new_row.names_attr@dimSelection <- list(row_selection)
                    llsel_selectors[["row.names"]] <- new_row.names_attr
                } else {
                    stop("Using non-Implicit selectors for row.names not ",
                         "supported for class 'RectSelector'")
                }
            }
            if("class" %in% attr_names) {
                h5ident <- llsel_selectors[["class"]]@h5identifier
                llsel_selectors[["class"]] <- AtomicSelector(h5file(h5ident),
                                                             h5root(h5ident))
            }
        } else {
            stop("decoding attrs for class '", class(sel), "' not supported")
        }
        llsel <- ListLikeSelector(selectors=llsel_selectors)
        decode_list_like(llsel, retain.names=TRUE, ...)
    }
}

.decode.name <- function(obj, sel, bookkeeping, ...) {
    attrs <- decode_attrs(sel, ...)
    data <- decode_data(sel, bookkeeping, ...)

    if(is.null(data))
        return(obj)
    ## coerce character to name
    data <- as.name(data)
    if(!is.null(attrs))
        attributes(data) <- attrs
    data
}

.decode.default <- function(obj, sel, bookkeeping, ...) {
    attrs <- decode_attrs(sel, ...)
    data <- decode_data(sel)

    if(is.null(data))
        return(obj)
    if(!is.null(attrs))
        attributes(data) <- attrs
    data
}

decode_S4 <- function(sel, bookkeeping)
{
    pkg <- bookkeeping[["package"]]
    nmspc <- if (identical(pkg, ".GlobalEnv")) {
        .GlobalEnv
    } else loadNamespace(pkg)
    class_name <- bookkeeping[["class"]]
    class_def <- getClass(class_name, where=nmspc)
    if (!identical(pkg, attr(class_def@className, "package")))
        stop("class '", class_name, "' not defined in package '", pkg, "'")

    ## initialize with initialize,ANY-method
    initANY <- getMethod(initialize, "ANY")
    proto_obj <- .Call(methods:::C_new_object, class_def)
    
    ## '.Data' slot referred to as "DataPart"; see ?getDataPart
    data_part <- decode_data(sel, bookkeeping)
    if(!is.null(data_part) && ! (".Data" %in% slotNames(proto_obj)))
        stop(".Data information retrieved for general S4 object",
             "(should not have .Data)")
    if(!is.null(data_part))
        slot(proto_obj, ".Data") <- data_part
    
    attrs <- decode_attrs(sel) ## list
    attributes(proto_obj) <- attrs

    proto_obj
}
