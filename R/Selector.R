.Implicit <- setClass("Implicit", slots=list(
                                    h5identifier="h5id"))
Implicit <- function(file, root) .Implicit(h5identifier=h5id(file, root))

.AtomicSelector <- setClass("AtomicSelector",
                            slots=list(
                              h5identifier="h5id",
                              mapper="character",
                              drop="logical",
                              dimMax="integer",
                              dimSelection="list",
                              h5attrs="ListLikeSelector"))

setMethod(show, "AtomicSelector", function(object) {
    cat("class: ", class(object), "\n")
    
    values <- lapply(slotNames(object), slot, object=object)
    names(values) <- slotNames(object)
    valnames <- slotNames(object)
    for(i in seq_along(values)) {
        if(valnames[[i]] == "h5identifier")
            print(values[[i]])
        if(valnames[[i]] %in% c("mapper", "drop", "dimMax"))
            cat(paste(valnames[[i]], values[[i]], sep=": "), collapse="\n")
        if(valnames[[i]] == "dimSelection") {
            ds <- lapply(values[[i]], function(x) unclass(as.which(x)))
            cat("dimSelection:\n")
            print(ds)
        }
        if(valnames[[i]] %in% c("h5attrs", "h5data")) {
            cat(paste0(valnames[[i]], ":"), "\n")
            print(values[[i]])
        }
    }
})

AtomicSelector <- function(file, root) {
    h5ident <- h5id(file, root)
    mapper <- generate_mapper(file, root)
    ## special treatment if has dim attr (e.g., matrix)
    if(has_dims_attr(file, root)) {
        dim_attr_path <- paste(root, "attrs/dim/data/data", sep="/")
        dims <- as.integer(h5read(file, dim_attr_path))
        dimMax <- dims
        dimSelection <- lapply(dimMax[[1L]], binit)
    } else { ## use dims dictated by underlying H5 object
        dimMax <- getdims(file, mapper[[1L]])
        dimSelection <- lapply(dimMax[[1L]], binit)
    }

    h5attrs <- ListLikeSelector(file, attrs_group(file, root), is.attrs=TRUE)
    
    .AtomicSelector(h5identifier=h5ident, mapper=mapper, drop=TRUE,
                    dimMax=dimMax, dimSelection=dimSelection, h5attrs=h5attrs)
}

.RecursiveSelector <- setClass("RecursiveSelector",
                               slots=list(
                                 h5identifier="h5id",
                                 selection_indices="integer",
                                 h5data="ListLikeSelector",
                                 h5attrs="ListLikeSelector"))
setMethod(show, "RecursiveSelector", function(object) {
    cat("class: ", class(object), "\n")

    values <- lapply(slotNames(object), slot, object=object)
    names(values) <- slotNames(object)
    valnames <- slotNames(object)
    for(i in seq_along(values)) {
        cat(paste0(valnames[[i]], ":"), "\n")
        print(values[[i]])
    }
})
RecursiveSelector <- function(file, root) {
    h5ident <- h5id(file, root)
    h5attrs <- ListLikeSelector(file, attrs_group(file, root), is.attrs=TRUE)
    h5data <- ListLikeSelector(file, data_group(file, root), is.attrs=FALSE)
    selection_indices <- seq_along(h5data@selectors)
    .RecursiveSelector(h5identifier=h5ident, selection_indices=selection_indices,
                       h5data=h5data, h5attrs=h5attrs)
}

.RectSelector <- setClass("RectSelector",
                          slots=list(
                            h5identifier="h5id",
                            col_selection="integer",
                            ## XXXX FIX ME: don't use integer for row_selection!
                            row_selection="integer",
                            h5data="ListLikeSelector",
                            h5attrs="ListLikeSelector"))
setMethod(show, "RectSelector", function(object) {
    cat("class: ", class(object), "\n")

    values <- lapply(slotNames(object), slot, object=object)
    names(values) <- slotNames(object)
    valnames <- slotNames(object)
    for(i in seq_along(values)) {
        cat(paste0(valnames[[i]], ":"), "\n")
        print(values[[i]])
    }
})
RectSelector <- function(file, root) {
    h5ident <- h5id(file, root)
    h5attrs <- ListLikeSelector(file, attrs_group(file, root), is.attrs=TRUE)
    h5data <- ListLikeSelector(file, data_group(file, root), all.implicit=TRUE)
    col_selection <- seq_along(h5data@selectors)
    row_selection <- integer()
    if(length(col_selection > 1L)) {
        ## Use a temporary Selector for first data element to get col lengths
        elt1_sel <- Selector(file, h5root(h5data@selectors[[1]]@h5identifier))
        row_selection <- seq_along(elt1_sel@dimSelection[[1]])
    }
    .RectSelector(h5identifier=h5ident, col_selection=col_selection,
                  row_selection=row_selection, h5data=h5data, h5attrs=h5attrs)
}

Selector <- function(file, root) {
    h5ident <- h5id(file, root)
    bkk <- decode_bookkeeping(h5ident)
    sxp_type <- bkk[["sexptype"]]
    cl_name <- bkk[["class"]]
    if(sxp_type == "VECSXP" || sxp_type == "S4SXP") {
        if(cl_name != "data.frame")
            RecursiveSelector(file, root)
        else
            RectSelector(file, root)
    } else {
        if(has_single_data_path(file, root))
            AtomicSelector(file, root)
        else
            Implicit(file, root)
    }
}

generate_mapper <- function(file, root) {
    if(has_single_data_path(file, root)) {
        single_data_path(file, root)
    } else {
        stop("Object at '", root, "' does not have data component at '",
             single_elt_datapath,
             "'. Did you mean to create a ListLikeSelector?")
    }
}
