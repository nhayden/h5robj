


decode_S4SXP <- function(h5file, path)
{
    ## read 'class' attribute (scalar character), including 'package'
    ## attribute on class (also scalar character)

    ## get class def
    if (!requireNamespace(package))
        stop("cannot decode object because pacakge '", pacakge,
             "'is not installed")
    classDef <- getClass(class, where=getNamespace(package))
    if (!identical(package, attr(class(classDef), "package")))
        stop("class '", class, "' not defined in package '", package, "'")

    ## read attributes other than 'class'

    ## initialize with initialize,ANY-method
    initANY <- getMethod(initialize, "ANY")
    do.call(initANY, c(list(classDef), otherAttrs))
}
