## temporary test file to be merged with others

suppressMessages({
    library(h5robj)
    library(RUnit)
})

test_unnamed_vector_Selector <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    vec <- 8:10
    encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    dimMax <- list(3L)
    dimSelection <- list(list(binit(3L)))
    sel_tar <- new("Selector", file=h5fl, root="foo", mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection,
                   h5attrs=list())
    ##print(sel); print(sel_tar)
    checkIdentical(sel_tar, sel)
}
##test_unnamed_vector_Selector()

test_named_vector_Selector <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    vec <- c(a=8, b=9, 10)
    encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")

    ## ## attrs-level (nested)
    ## dimMax <- list(3L)
    ## dimSelection <- list(list(binit(3L)))
    ## attr_tar <- new("Selector", file=h5fl, root="foo/attrs/names",
    ##                 mapper="foo/attrs/names/data/data", drop=TRUE, dimMax=dimMax,
    ##                 dimSelection=dimSelection, h5attrs=list())
    ## top-level
    dimMax <- list(3L)
    dimSelection <- list(list(binit(3L)))
    sel_tar <- new("Selector", file=h5fl, root="foo", mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection,
                   h5attrs=list(names=Placeholder()))
    ##print(sel); print(sel_tar)
    checkIdentical(sel_tar, sel)
}
##test_named_vector_Selector()
