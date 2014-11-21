## only expected to work for datasets and groups
.isAbsent <- function(file, name) {
    loc = rhdf5:::h5checktypeOrOpenLoc(file, readonly=TRUE)
    res <- !H5Lexists(loc$H5Identifier, name)
    rhdf5:::h5closeitLoc(loc)
    res
}

h5exists <- function(file, name) {
    loc = rhdf5:::h5checktypeOrOpenLoc(file, readonly=TRUE)
    res <- H5Lexists(loc$H5Identifier, name)
    rhdf5:::h5closeitLoc(loc)
    res
}

has_dims_attr <- function(file, name) {
    ## or should only check attrs/dim? Could be NULL in that case?
    dims_path <- paste(name, "attrs/dim/data/data", sep="/")
    h5exists(file, dims_path)
}

.has_attr_x <- function(file, path, attrname) {
    attrs_path <- attrs_group(file, path)
    h5exists(file, paste(attrs_path, attrname, sep="/"))
}
setGeneric("has_attr_x", function(h5id, attrname) standardGeneric("has_attr_x"))
setMethod("has_attr_x", c("h5id", "character"), function(h5id, attrname) {
    .has_attr_x(h5file(h5id), h5root(h5id), attrname)
})    

has_attrs_group <- function(file, name) {
    h5exists(file, paste(name, "attrs", sep="/"))
}
attrs_group <- function(file, name, checked=FALSE) {
    attrs_path <- paste(name, "attrs", sep="/")
    if(!checked) return(attrs_path)
    if(h5exists(file, attrs_path)) attrs_path else NULL
}
has_data_group <- function(file, name) {
    h5exists(file, paste(name, "data", sep="/"))
}
data_group <- function(file, name, checked=FALSE) {
    data_path <- paste(name, "data", sep="/")
    if(!checked) return(data_path)
    if(h5exists(file, data_path)) data_path else NULL
}
has_single_data_path <- function(file, name) {
    h5exists(file, paste(name, "data/data", sep="/"))
}
single_data_path <- function(file, name, checked=FALSE) {
    data_data_path <- paste(name, "data/data", sep="/")
    if(!checked) return(data_data_path)
    if(h5exists(file, data_data_path)) data_data_path else NULL
}
is_empty_obj <- function(file, name) {
    (!has_attrs_group(file, name)) && (!has_data_group(file, name))
}

## Rtype values that should not have class attribute explicitly set
.classless_types <- c("logical", "integer", "double", "character", "raw",
                      "list", "NULL", "symbol")

## representation of placeholder symbol for *ORNULL slots on S4 objs
.pseudoNULL <- as.name("\001NULL\001")

h5type <- function(x) {
    switch(typeof(x),
           ## primitives
           logical="LGLSXP", integer="INTSXP", double="REALSXP",
           character="STRSXP", raw="RAWSXP",
           ## other
           S4="S4SXP", list="VECSXP", NULL="NILSXP", symbol="SYMSXP",
           stop("unhandled type '", typeof(x), "'"))
}

Rtype <- function(x) {
    switch(x,
           ## primitives
           LGLSXP="logical", INTSXP="integer", REALSXP="double",
           STRSXP="character", RAWSXP="raw",
           ## other
           S4SXP="S4", VECSXP="list", NILSXP="NULL", SYMSXP="symbol",
           stop("unhandled h5type '", x, "'"))
}

.create_temp_h5 <- function() {
    h5fl <- tempfile()
    if(interactive())
        message(h5fl)
    stopifnot(h5createFile(h5fl))
    ## return path to temp h5
    h5fl
}

getdims <- function(file, name) {
    fid <- H5Fopen(file)
    oid <- H5Oopen(fid, name)
    ## will throw error if not a dataset (invisibly returns NULL);
    ## seems more diagnostic than the error if H5Dopen is not a
    ## dataset.
    rhdf5:::h5checktype(oid, "dataset")
    dataspace <- H5Dget_space(oid)
    dims <- H5Sget_simple_extent_dims(dataspace)
    extents <- dims$size
    H5Fclose(fid)
    H5Oclose(oid)
    H5Sclose(dataspace)
    extents
}

all_paths_from_h5 <- function(filename) {
    h5l <- h5ls(filename)
    paths <- character(length=nrow(h5l))
    for(i in seq_len(nrow(h5l))) {
        if(h5l[i,"group"] == "/")
            path <- paste0(h5l[i,"group"], h5l[i,"name"])
        else
            path <- paste(h5l[i,"group"], h5l[i,"name"], sep="/")
        ## prevent double leading '/'
        ## path <- sub("^\\/", "", path)
        paths[i] <- path
    }
    paths
}

h5ls_immediate_descendants <- function(file, name) {
    if(!h5exists(file, name))
        stop("object '", name, "' does not exist")
    prepended_slash <- FALSE
    if(substr(name, 1L, 1L) != "/") {
        name <- paste0("/", name)
        prepended_slash <- TRUE
    }
    pattern <- paste0("^", name)
    paths <- all_paths_from_h5(file)
    matches <- grep(pattern, paths, value=TRUE)
    input_depth <- length(strsplit(name, "/")[[1]])
    matches_lens <- lapply(strsplit(matches, "/"), length)
    imm_descendants <- matches[matches_lens == (input_depth + 1)]
    if(prepended_slash)
        substring(imm_descendants, 2L)
    else
        imm_descendants
}

## used by decode to infer the names of list elements
infer_list_names <- function(paths) {
    if(is.null(paths) || length(paths) == 0L)
        stop("paths must be non-zero")
    basename(paths)
}

## bit object initialized to all TRUE; do this in C?
binit <- function(length) {
    b = bit(as.integer(length))
    b[] <- TRUE
    b
}
