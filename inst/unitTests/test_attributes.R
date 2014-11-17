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

test_unnamed_list_without_data_RSelector <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list()
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    h5ident <- h5id(h5fl, "foo")
    h5data <- h5robj:::.ListLikeSelector()
    h5attrs <- h5robj:::.ListLikeSelector()
    tar <- h5robj:::.RecursiveSelector(h5identifier=h5ident,
                                       h5data=h5data,
                                       h5attrs=h5attrs)
    ##print(sel); print(tar)
    checkIdentical(tar, sel)  
}
##test_unnamed_list_without_data_RSelector()

test_unnamed_list_RSelector <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(42, "yeehaw")
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    drop <- TRUE
    dimMax <- 1L
    dimSelection <- list(binit(1L))
    data_roots <- c("foo/data/elt1", "foo/data/elt2")
    mappers <- sapply(data_roots, paste, "data/data", sep="/")
    elt1_h5ident <- h5id(h5fl, data_roots[[1]])
    elt1_sel <- h5robj:::.AtomicSelector(h5identifier=elt1_h5ident,
                                         mapper=mappers[[1]],
                                         drop=drop, dimMax=dimMax,
                                         dimSelection=dimSelection)
    elt2_h5ident <- h5id(h5fl, data_roots[[2]])
    elt2_sel <- h5robj:::.AtomicSelector(h5identifier=elt2_h5ident,
                                         mapper=mappers[[2]],
                                         drop=drop, dimMax=dimMax,
                                         dimSelection=dimSelection)
    h5data <- ListLikeSelector(selectors=list(elt1_sel, elt2_sel))
    h5attrs <- ListLikeSelector(selectors=list())
    top_h5ident <- h5id(h5fl, "foo")
    selection_indices <- seq_along(l)
    tar <- h5robj:::.RecursiveSelector(h5identifier=top_h5ident,
                                       selection_indices=selection_indices,
                                       h5data=h5data, h5attrs=h5attrs)
    ##print(res); print(tar)
    checkIdentical(tar, sel)  
}
##test_unnamed_list_RSelector()

test_named_list_RSelector <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(a=42, funtastic="yeehaw")
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")

    #h5data
    drop <- TRUE
    dimMax <- 1L
    dimSelection <- list(binit(1L))
    data_roots <- c("foo/data/elt1", "foo/data/elt2")
    mappers <- sapply(data_roots, paste, "data/data", sep="/")
    elt1_h5ident <- h5id(h5fl, data_roots[[1]])
    elt1_sel <- h5robj:::.AtomicSelector(h5identifier=elt1_h5ident,
                                         mapper=mappers[[1]],
                                         drop=drop, dimMax=dimMax,
                                         dimSelection=dimSelection)
    elt2_h5ident <- h5id(h5fl, data_roots[[2]])
    elt2_sel <- h5robj:::.AtomicSelector(h5identifier=elt2_h5ident,
                                         mapper=mappers[[2]],
                                         drop=drop, dimMax=dimMax,
                                         dimSelection=dimSelection)
    h5data <- ListLikeSelector(selectors=list(elt1_sel, elt2_sel))

    #h5attrs
    names_h5ident <- h5id(h5fl, "foo/attrs/names")
    names_sel <- h5robj:::.Implicit(h5identifier=names_h5ident)
    h5attrs <- ListLikeSelector(selectors=list(names=names_sel))

    top_h5ident <- h5id(h5fl, "foo")
    selection_indices <- seq_along(l)
    tar <- h5robj:::.RecursiveSelector(h5identifier=top_h5ident,
                                       selection_indices=selection_indices,
                                       h5data=h5data, h5attrs=h5attrs)
    ##print(res); print(tar)
    checkIdentical(tar, sel)  
}
##test_named_list_RSelector()

test_unnamed_list_RSelector_whole <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(6L, 40:42, letters[15:18])
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    res <- mat(sel)
    ##print(res); print(l)
    checkIdentical(l, res)  
}
##test_unnamed_list_RSelector_whole()

test_unnamed_list_RSelector_subsetted <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(6L, 40:42, letters[15:18])
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2:3]
    res <- mat(sel2)
    tar <- l[2:3]
    ##print(res); print(tar)
    checkIdentical(tar, res)  
}
##test_unnamed_list_RSelector_subsetted()

## focus here is keeping names in sync, converting Implicit PROBLEM:
## in order to convert Implicit names, have to know *which* elements
## of h5data were selected, not just the length of h5data
test_named_list_RSelector_whole <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(a=6L, fun=40:42, tastic=letters[15:18])
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    res <- mat(sel)
    ##print(res); print(l)
    checkIdentical(l, res)  
}
##test_named_list_RSelector_whole()

test_named_list_RSelector_subsetted <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    l <- list(a=6L, fun=40:42, tastic=letters[15:18])
    h5robj::encode(l, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2:3]
    res <- mat(sel2)
    tar <- l[2:3]
    ##print(res); print(tar)
    checkIdentical(tar, res)  
}
##test_named_list_RSelector_subsetted()
