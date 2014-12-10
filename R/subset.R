setMethod("[", c("RecursiveSelector", "numeric"),
    function(x, i) {
        i <- as.integer(i)
        h5data <- x@h5data@selectors[i]
        selection_indices <- x@selection_indices[i]
        initialize(x, selection_indices=selection_indices,
                   h5data=ListLikeSelector(selectors=h5data))
    }
)

setMethod("[", c("RectSelector", "ANY"),
    function(x, i, j) {
        h5data <- x@h5data@selectors[j]
        row_selection <- x@row_selection[i]
        col_selection <- x@col_selection[j]
        initialize(x, row_selection=row_selection, col_selection=col_selection,
                   h5data=ListLikeSelector(selectors=h5data))
    }
)

##setMethod("[", c("Selector", "numeric", "missing", "ANY"),
setMethod("[", c("AtomicSelector", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
        ndims <- length(x@dimMax)
        nsubscripts <- if(missing(drop)) nargs() - 1L else nargs() - 2L
        if(nsubscripts != ndims) {
            stop(
                sprintf("dim mismatch: obj has %d dims but %d %s specified",
                        ndims, nsubscripts,
                        if(nsubscripts == 1L) "was" else "were")
                 )
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
            if(imax > x@dimMax[[dimi]]) {
                stop("max index for dim ", dimi, " exceeds upper limit ('",
                     x@dimMax[[dimi]], "')")
            }
        }

        ## do these steps with one dim at a time, since potentially large
        dimSelection <- x@dimSelection
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

        initialize(x, dimSelection=dimSelection, drop=drop)
    }
)

setGeneric("mat", function(obj) {
    standardGeneric("mat")
})
setMethod("mat", "RectSelector", function(obj) {
    decodeSel(obj)
})
setMethod("mat", "RecursiveSelector", function(obj) {
    decodeSel(obj)
})
setMethod("mat", "Implicit", function(obj) {
    decodeSel(obj)
})
setMethod("mat", "AtomicSelector", function(obj) {
    decodeSel(obj)
})
