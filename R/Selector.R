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

    path_to_attrs_group <- paste(root, "attrs", sep="/")
    h5attrs <- ListLikeSelector(file, path_to_attrs_group, is.attrs=TRUE)
    
    .AtomicSelector(h5identifier=h5ident, mapper=mapper, drop=TRUE,
                    dimMax=dimMax, dimSelection=dimSelection, h5attrs=h5attrs)
}

.RecursiveSelector <- setClass("RecursiveSelector",
                               slots=list(
                                 h5identifier="h5id",
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
    attrs_group_path <- paste(root, "attrs", sep="/")
    data_group_path <- paste(root, "data", sep="/")
    h5attrs <- ListLikeSelector(file, attrs_group_path, is.attrs=TRUE)
    h5data <- ListLikeSelector(file, data_group_path, is.attrs=FALSE)
    .RecursiveSelector(h5identifier=h5ident, h5data=h5data, h5attrs=h5attrs)
}

Selector <- function(file, root) {
    h5ident <- h5id(file, root)
    bkk <- decode_bookkeeping(h5ident)
    sxp_type <- bkk[["sexptype"]]
    if(sxp_type == "VECSXP")
        RecursiveSelector(file, root)
    else
        AtomicSelector(file, root)
}

generate_mapper <- function(file, root) {
    single_elt_datapath <- paste(root, "data", "data", sep="/")
    if(h5robj:::h5exists(file, single_elt_datapath)) {
        single_elt_datapath
    } else {
        stop("Object at '", root, "' does not have data component at '",
             single_elt_datapath,
             "'. Did you mean to create a ListLikeSelector?")
        ## all_paths <- h5robj:::all_paths_from_h5(file)
        ## ## identify the paths for the numbered "elt"s under root
        ## elt_paths = grep(
        ##   paste0(paste(root, "data", "elt", sep="/"), "[[:digit:]]+$"),
        ##   all_paths, value=TRUE)
        ## elt_paths
    }
}