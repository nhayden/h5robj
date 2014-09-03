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

## Rtype values that should not have class attribute explicitly set
.classless_types <- c("logical", "integer", "double", "character", "raw",
                      "list", "NULL", "symbol")

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

h5ls_immeditate_descendants <- function(file, name) {
    if(!h5exists(file, name))
        stop("object '", name, "' does not exist")
    if(substr(name, 1L, 1L) != "/")
        name <- paste0("/", name)
    pattern <- paste0("^", name)
    paths <- all_paths_from_h5(file)
    matches <- grep(pattern, paths, value=TRUE)
    input_depth <- length(strsplit(name, "/")[[1]])
    matches_lens <- lapply(strsplit(matches, "/"), length)
    imm_descendants <- matches[matches_lens == (input_depth + 1)]
}
