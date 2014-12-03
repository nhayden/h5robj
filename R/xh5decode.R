## decode_bookkeeping <- function(file, name, ...) {
##     bookkeeping <- lapply(h5readAttributes(file, name), as.character)
##     H5close()
##     bookkeeping
## }

## decode <- function(file, name, ...) {
##     bkk <- decode_bookkeeping(file, name, ...)
##     bkk_names <- names(bkk)
##     if( !("class" %in% bkk_names) || !("sexptype" %in% bkk_names) )
##         stop("Can't decode; missing bookkeeping information in file")
##     if( bkk[["sexptype"]] == "S4SXP" && !("package" %in% bkk_names) )
##         stop("Can't decode; S4 object without package information")

##     if("package" %in% bkk_names)
##         decode_S4(file, name, bkk)
##     else
##         decode_S3(file, name, bkk)
## }

## decode_S3 <- function(file, name, bookkeeping) {
##     cl_name <- bookkeeping[["class"]]
##     nugget_sexptype <- Rtype(bookkeeping[["sexptype"]])
##     if( !(nugget_sexptype %in% .classless_types) ) {
##         stop("decoding S3 for type '", cl_name, "' not implemented")
##     }
##     proto_obj <- NULL
##     if(cl_name == "NULL") {
##         return(proto_obj)
##     } else if(cl_name == "name") { ## 'name' / 'symbol'
##         ## R's pseudo NULL object
##         proto_obj <- as.name('\001NULL\001')
##     } else if(nugget_sexptype %in% .classless_types) {
##         proto_obj <- vector(nugget_sexptype)
##     } else {
##         proto_obj <- structure(vector(nugget_sexptype), class=cl_name)
##     }
##     .decode(proto_obj, file, name, bookkeeping)
## }

## .decode <- function(obj, file, name, bookkeeping, ...) {
##     UseMethod(".decode")
## }

## decode_list_like <- function(file, name, retain.names=FALSE, ...) {
##     list_elts <- h5ls_immediate_descendants(file, name)
##     res <- vector('list', length(list_elts))
##     if(retain.names) {
##         ## FIX ME: include check that names attribute exists in H5?
##         names(res) <- infer_list_names(list_elts)
##     }
##     for(i in seq_along(list_elts)) {
##         res[[i]] <- decode(file, list_elts[[i]], ...)
##     }
##     res
## }

## read_nugget <- function(file, name, bookkeeping, ...) {
##     nugget_name <- paste(name, "data", sep="/")
##     if(!h5exists(file, nugget_name))
##         stop("data nugget for '", name, "' does not exist in file")
##     as.vector(h5read(file, nugget_name))
## }

## decode_data <- function(file, name, bookkeeping, ...) {
##     data_name <- paste(name, "data", sep="/")
##     if(!h5exists(file, data_name)) {
##         NULL
##     } else {
##         if(bookkeeping[["sexptype"]] == "VECSXP") {
##             decode_list_like(file, data_name, ...)
##         } else {
##             read_nugget(file, data_name, bookkeeping, ...)
##         }
##     }
## }

## decode_attrs <- function(file, name, bookkeeping, ...) {
##     attrs_name <- paste(name, "attrs", sep="/")
##     if(!h5exists(file, attrs_name)) {
##         NULL
##     } else {
##         decode_list_like(file, attrs_name, retain.names=TRUE, ...)
##     }
## }

## .decode.name <- function(obj, file, name, bookkeeping, ...) {
##     attrs <- decode_attrs(file, name, ...)
##     data <- decode_data(file, name, bookkeeping, ...)

##     if(is.null(data))
##         return(obj)
##     ## coerce character to name
##     data <- as.name(data)
##     if(!is.null(attrs))
##         attributes(data) <- attrs
##     data
## }

## .decode.default <- function(obj, file, name, bookkeeping, ...) {
##     attrs <- decode_attrs(file, name, ...)
##     data <- decode_data(file, name, bookkeeping, ...)

##     if(is.null(data))
##         return(obj)
##     if(!is.null(attrs))
##         attributes(data) <- attrs
##     data
## }

## decode_S4 <- function(file, name, bookkeeping)
## {
##     pkg <- bookkeeping[["package"]]
##     nmspc <- if (identical(pkg, ".GlobalEnv")) {
##         .GlobalEnv
##     } else loadNamespace(pkg)
##     class_name <- bookkeeping[["class"]]
##     class_def <- getClass(class_name, where=nmspc)
##     if (!identical(pkg, attr(class_def@className, "package")))
##         stop("class '", class_name, "' not defined in package '", pkg, "'")

##     ## initialize with initialize,ANY-method
##     initANY <- getMethod(initialize, "ANY")
##     proto_obj <- .Call(methods:::C_new_object, class_def)
    
##     ## '.Data' slot referred to as "DataPart"; see ?getDataPart
##     data_part <- decode_data(file, name, bookkeeping)
##     if(!is.null(data_part) && ! (".Data" %in% slotNames(proto_obj)))
##         stop(".Data information retrieved for general S4 object",
##              "(should not have .Data)")
##     if(!is.null(data_part))
##         slot(proto_obj, ".Data") <- data_part
    
##     attrs <- decode_attrs(file, name) ## list
##     attributes(proto_obj) <- attrs

##     proto_obj
## }
