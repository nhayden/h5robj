.Selector <- setClass("Selector", slots=list(
                         file="character",
                         root="character",
                         mapper="character",
                         drop="logical",
                         dimMax="list",
                         dimSelection="list",
                         h5attrs="list")) ## nested Selectors for attributes

.Placeholder <- setClass("Placeholder", slots=list(ignore="raw"))
Placeholder <- function() .Placeholder(ignore=raw())

.AllS <- setClass("AllS", slots=list(file="character", root="character"))
AllS <- function(file, root) {
    if(!file.exists(file))
        stop("file does not exist: ", file)
    if(.isAbsent(file, root))
        stop("path '", root, "' in file '", file, "' does not exist")
    .AllS(file=file, root=root)
}

generate_h5attributes <- function(file, root) {
    path_to_attrs_group <- paste(root, "attrs", sep="/")
    if(!h5exists(file, path_to_attrs_group))
        return(list())
    attr_paths <- h5ls_immediate_descendants(file, path_to_attrs_group)
    attr_names <- basename(attr_paths)
    h5attrs <- lapply(attr_paths, function(x) Selector(file, x))
    names(h5attrs) <- attr_names
    ## add more to right operand of %in% as they become relevant
    h5attrs[names(h5attrs) %in% "names"] = Placeholder()
    h5attrs
}

## only works for non-recursive objs
Selector <- function(file, root) {
    if(!file.exists(file))
        stop("file does not exist: ", file)
    mapper <- generate_mapper(file, root)
    ## special treatment if has dim attr (e.g., matrix)
    if(has_dims_attr(file, root)) {
        dim_attr_path <- paste(root, "attrs/dim/data/data", sep="/")
        dims <- as.integer(h5read(file, dim_attr_path))
        dimMax <- list(dims)
        dimSelection <- list(sapply(dimMax[[1L]], binit))
    } else { ## use dims dictated by underlying H5 object
        dimMax <- list(getdims(file, mapper[[1L]]))
        dimSelection <- list(sapply(dimMax[[1L]], binit))
    }
    
    .Selector(file=file, root=root, mapper=mapper, drop=TRUE, dimMax=dimMax,
              dimSelection=dimSelection,
              h5attrs=generate_h5attributes(file, root))
}

##setMethod("[", c("Selector", "numeric", "missing", "ANY"),
setMethod("[", c("Selector", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
        ndims <- length(x@dimMax[[1L]])
        nsubscripts <- if(missing(drop)) nargs() - 1L else nargs() - 2L
        if(nsubscripts != ndims) {
            stop("dim mismatch: obj has ", ndims, " dims, but ",
                 nsubscripts, " were specified")
        }
        selections <- vector('list', ndims)
        if(!missing(i)) {
            if(!is.numeric(i))
                stop("subscript 'i' must be numeric if provided; got ", class(i))
            selections[[1L]] <- as.integer(i)
        }
        if(!missing(j)) {
            if(!is.numeric(j))
                stop("subscript 'j' must be numeric if provided; got ", class(j))
            selections[[2L]] <- as.integer(j)
        }
        ## checking '...'
        if(!missing(...)) {
            ## hack: use the fact that class of missing values in '...'
            ## are "name" objects
            dot_subs <- substitute(...())
            empty_dots <- sapply(dot_subs, function(x) {
                class(x) == "name" && as.character(x) == ""
            })
            drop_dots <- which(empty_dots)
            mcall <- match.call()
            ## sequence of '...' indices
            seqn <- seq_along(dot_subs)
            ## less those with missing values (if applicable)
            if(length(drop_dots) > 0L)
                seqn <- seqn[-drop_dots]
            ## offset in match.call() to the first '...' arg
            dot_offset <- 4L ## if i and j are provided (non-empty)
            if(missing(i)) dot_offset <- dot_offset - 1L
            if(missing(j)) dot_offset <- dot_offset - 1L
            for(doti in seqn) {
                ## XXXX FIX ME: -3 not reliable?
                if(class(dot_subs[[doti]]) == "name")
                    evald <- eval(mcall[[doti + dot_offset]], -3)
                else
                    evald <- eval(mcall[[doti + dot_offset]])
                actual_subscript <- doti + 2L
                if(!is.numeric(evald)) {
                    stop("subscript ", actual_subscript,
                         " must be numeric if provided; got ", class(evald))
                }
                selections[[actual_subscript]] <- as.integer(evald)
            }
        }
        ## selections is a list with number elts equal to number of
        ## dimensions of underlying object; NULL values are used when
        ## indices are not supplied for a dimension

        ## only need to do bounds checking and update values for
        ## non-empty subscripts
        nonnull <- which(sapply(selections, Negate(is.null)))
        for(dimi in nonnull) {
            imax <- max(selections[[dimi]])
            if(imax > x@dimMax[[1L]][[dimi]]) {
                stop("max index for dim ", dimi, " exceeds upper limit ('",
                     x@dimMax[[1L]][[dimi]], "')")
            }
        }

        ## do these steps with one dim at a time, since potentially large
        dimSelection <- x@dimSelection[[1L]]
        for(dimi in nonnull) {
            ## get previously selected indices
            idx <- as.which(dimSelection[[dimi]])

            ## convert new selection to bit, big enough to
            ## accom. previously selected range
            biti <- as.bit.which(selections[[dimi]], length=length(idx))

            if(sum(biti) == 0L) ## wipe all selections if none selected
                dimSelection[[dimi]][] <- FALSE
            else
                dimSelection[[dimi]][idx] <-
                    dimSelection[[dimi]][idx] & biti
        }

        initialize(x, dimSelection=list(dimSelection), drop=drop)
    }
)

setGeneric("mat", function(obj) {
    standardGeneric("mat")
})

linearize <- function(obj) {
    len <- prod(obj@dimMax[[1L]])
    linear <- bit(len)

    ## extent of each dimension (equivalent to calling 'dim' on array-like)
    d <- obj@dimMax[[1L]]
    ## w for 'which(es)'
    w <- lapply(obj@dimSelection[[1L]], function(x) as.which(x))
    k <- length(w)
    ## idx starts as indices from last subscript
    idx <- w[[length(w)]]
    while(k > 1L) {
        k <- k - 1L
        idx <- outer(w[[k]] - 1L, (idx - 1L) * d[k], `+`) + 1L
    }
    
    linear[idx] <- TRUE
    linear
}

multidimmat <- function(obj) {
    ## for now, require all encoded multidim objects to have dim attr
    if(!has_dims_attr(obj@file, obj@root))
        stop("expected obj at ", obj@root, " to have dims attr")

    linearselection <- linearize(obj)
    idx <- as.which(linearselection)
    tmp <- as.vector(h5read(obj@file, obj@mapper[[1L]],
                            index=list(idx)))
    dim(tmp) <- sapply(obj@dimSelection[[1L]], sum)
    if(obj@drop)
        tmp <- drop(tmp)
    tmp
}

## doesn't handle zero-length objs
## doesn't handle recursive objs
setMethod("mat", "Selector", function(obj) {
    ## need to branch here if atomic vs. recursive (replacing tmp
    ## vs. filling in elements of tmp list)
    if(length(obj@dimMax[[1L]]) > 1L)
        return(multidimmat(obj))
    if(length(obj@mapper) == 0L) {
        stop("subsetting zero-length objects not supported")
    } else if(length(obj@mapper) > 1L) { ## recursive (e.g., list)
        stop("recursive types not currently supported")
    } else { ## atomic
        if(sum(obj@dimSelection[[1L]][[1L]]) == 0L) {
        ## XXXX FIX ME: HACK! selects the first element then subsets
        ## with 0. What really needs to happen here is to create an
        ## empty vector of the right type to hang attributes on.
        tmp <- as.vector(h5read(obj@file, obj@mapper[[1L]],
                                index=list(1)))[0]
        } else {
            idx <- as.which(obj@dimSelection[[1L]][[1L]])
            tmp <- as.vector(h5read(obj@file, obj@mapper[[1L]],
                                    index=list(idx)))
        }
    }
    ## getting attrs out
    if(length(obj@h5attrs) == 0L)
        return(tmp)
    attrs_values <- obj@h5attrs
    ## for(elt in seq_along(attrs_values)) {
    ##     ## handle names specially
    ##     if( ! grepl("/names$", attrs_paths[[elt]]) )
    ##         attrs_values[[elt]] <- decode(obj@file, attrs_paths[[elt]])
    ##     else {
    ##         if(sum(obj@dimSelection[[1L]][[1L]]) > 0L) {
    ##             idx <- as.which(obj@dimSelection[[1L]][[1L]])
    ##             name_name <- paste(attrs_paths[[elt]], "data/data", sep="/")
    ##             attrs_values[[elt]] <- as.vector(h5read(obj@file, name_name,
    ##                                                 index=list(idx)))
    ##         }
    ##         else { ## names attribute must *appear*, even if empty
    ##             attrs_values[[elt]] <- character(0)
    ##         }
    ##     }
    ## }
    ## names(attrs_values) <- basename(attrs_paths)
    for(elt in seq_along(attrs_values)) {
        ## use selections of top-level object if Placeholder for attribute
        if(is(attrs_values[[elt]], "Placeholder")) {
            if(sum(obj@dimSelection[[1L]][[1L]]) > 0) {
                idx <- as.which(obj@dimSelection[[1L]][[1L]])
                attrpath <- paste(obj@root, "attrs", names(attrs_values)[[elt]],
                                  "data/data", sep="/")
                attrs_values[[elt]] <- as.vector(h5read(obj@file, attrpath,
                                                       index=list(idx)))
            }
        } else {
            attrSelector <- attrs_values[[elt]]
            if(sum(attrSelector@dimSelection[[1L]][[1L]]) > 0L) {
                idx <- as.which(attrSelector@dimSelection[[1L]][[1L]])
                attrpath <- paste(attrSelector@root, "data/data", sep="/")
                attrs_values[[elt]] <- as.vector(h5read(obj@file, attrpath,
                                                        index=list(idx)))
            }
        }
    }

    attributes(tmp) <- attrs_values
    tmp
})

generate_mapper <- function(file, root) {
    single_elt_datapath <- paste(root, "data", "data", sep="/")
    if(h5robj:::h5exists(file, single_elt_datapath)) {
        single_elt_datapath
    } else {
        all_paths <- h5robj:::all_paths_from_h5(file)
        ## identify the paths for the numbered "elt"s under root
        elt_paths = grep(
          paste0(paste(root, "data", "elt", sep="/"), "[[:digit:]]+$"),
          all_paths, value=TRUE)
        elt_paths
    }
}
