## temporary test file to be merged with others

suppressMessages({
    library(h5robj)
    library(RUnit)
})

test_unnamed_vector_ASelector <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    vec <- 8:10
    encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    dimMax <- 3L
    dimSelection <- list(binit(3L))
    h5ident <- h5id(h5fl, "foo")
    h5attrs <- ListLikeSelector(selectors=list())
    sel_tar <- new("AtomicSelector", h5identifier=h5ident,
                   mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection,
                   h5attrs=h5attrs)
    ##print(sel); print(sel_tar)
    checkIdentical(sel_tar, sel)
}
##test_unnamed_vector_ASelector()

test_named_vector_ASelector <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    vec <- c(a=8, b=9, 10)
    encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")

    dimMax <- 3L
    dimSelection <- list(binit(3L))
    h5ident <- h5id(h5fl, "foo")
    h5attrs <- ListLikeSelector(selectors=list(
                                  names=Implicit(h5fl, "foo/attrs/names")))
    sel_tar <- new("AtomicSelector", h5identifier=h5ident,
                   mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection,
                   h5attrs=h5attrs)
    ##print(sel); print(sel_tar)
    checkIdentical(sel_tar, sel)
}
##test_named_vector_ASelector()

## without subsetting. testing ListLikeSelector
test_named_vector_whole <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    
    sel <- Selector(file=h5fl, root="foo")
    res <- mat(sel)
    ##print(res); print(vec)
    checkIdentical(vec, res)
}
##test_named_vector_whole()

test_named_vector_subsetted <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    
    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2:3]
    res <- mat(sel2)
    tar <- vec[2:3]
    ##print(res); print(tar)
    checkIdentical(tar, res)
}
##test_named_vector_subsetted()

## Old comment:
## THIS IS WRONG. Unexpected behavior because using "[" on an object
## SHOULD drop all attributes except 'names', 'dim', and
## 'dimnames'. See ?"["
## -------------------------
## Rewriting to just subset the attribute
test_named_vector_with_subsetted_attribute <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    attr(vec, "fabulous") <- 21:25
    h5robj::encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    sel@h5attrs@selectors["fabulous"] <- sel@h5attrs@selectors[["fabulous"]][2:4]
    res <- mat(sel)
    tar <- vec
    attr(tar, "fabulous") <- attributes(vec)[["fabulous"]][2:4]
    ##print(res); print(tar)
    checkIdentical(tar, res)  
}
##test_named_vector_with_subsetted_attribute()
