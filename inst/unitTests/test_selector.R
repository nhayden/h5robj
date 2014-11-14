suppressMessages({
    library(h5robj)
    library(RUnit)
})

.named_matrix <- function() {
    matrix(1:6, nrow=2L, dimnames=list(c("a", "b"), c("x", "y", "z")))
}
.unnamed_matrix <- function() {
    matrix(1:6, nrow=2L)
}
.unnamed_array <- function() {
    array(51:74, 2:4)
}

test_unnamed_matrix_Selector <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    m <- .unnamed_matrix()
    encode(m, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    dimMax <- list(c(2L, 3L))
    dimSelection <- list(list(binit(2L), binit(3L)))
    sel_tar <- new("Selector", file=h5fl, root="foo", mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection)
    ##print(sel); print(sel_tar)
    checkIdentical(sel_tar, sel)
}
##test_unnamed_matrix_Selector()

test_unnamed_matrix_subset <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    m <- .unnamed_matrix()
    encode(m, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2, 2:3]

    dimMax <- list(c(2L, 3L))
    b1 <- as.bit(c(FALSE, TRUE))
    b2 <- as.bit(c(FALSE, TRUE, TRUE))
    dimSelection <- list(list(b1, b2))
    sel_tar <- new("Selector", file=h5fl, root="foo", mapper="foo/data/data",
                   drop=TRUE, dimMax=dimMax, dimSelection=dimSelection)
    checkIdentical(sel_tar, sel2)
}
##test_unnamed_matrix_subset()

test_linearize_2d <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    m <- .unnamed_matrix()
    encode(m, h5fl, "foo")

    idx <- list(1:2, 3)
    sel <- Selector(file=h5fl, root="foo")
    sel <- do.call("[", c(list(sel), idx))
    lin <- linearize(sel)
    res <- array(as.logical(lin), dim=dim(m))

    tar <- array(FALSE, dim=dim(m))
    ## set idx elts of tar to TRUE
    tar <- do.call("[<-", c(list(tar), idx, TRUE))

    ##print(tar); print(res)
    checkIdentical(tar, res)
}
##test_linearize_2d()

test_linearize_3d_all_specified <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    a <- .unnamed_array()
    encode(a, h5fl, "foo")

    idx <- list(1:2, 2:3, 1:3)
    sel <- Selector(file=h5fl, root="foo")
    sel <- do.call("[", c(list(sel), idx))
    lin <- linearize(sel)
    res <- array(as.logical(lin), dim=dim(a))

    tar <- array(FALSE, dim=dim(a))
    ## set idx elts of tar to TRUE
    tar <- do.call("[<-", c(list(tar), idx, TRUE))

    ##print(tar); print(res)
    checkIdentical(tar, res)
}
##test_linearize_3d_all_specified()

test_linearize_3d_missing_dots <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    a <- .unnamed_array()
    encode(a, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")[1:2, 2:3, ]
    lin <- linearize(sel)
    res <- array(as.logical(lin), dim=dim(a))

    tar <- array(FALSE, dim=dim(a))
    ## set idx elts of tar to TRUE
    tar[1:2, 2:3, ] <- TRUE
    ##print(tar); print(res)
    checkIdentical(tar, res)
}
##test_linearize_3d_missing_dots()

## test for error when i or j args are missing; earlier error in
## calculating the match.call() offset to first element of '...'
test_subset_missing_ij <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    a <- .unnamed_array()
    encode(a, h5fl, "foo")
    
    sel <- Selector(file=h5fl, root="foo")
    sel_noi <- sel[1, , 3]
    checkIdentical(a[1, , 3], mat(sel_noi))
    sel_noj <- sel[, 2, 3]
    checkIdentical(a[, 2, 3], mat(sel_noj))
}
##test_subset_missing_ij()

## test effect of variable names on
## sapply(substitute(...()), function(x) {
##     class(x) == "name" && as.character(x)
## })
## when checking for missingness
test_subset_variable_names <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    a <- array(1:120, c(2, 3, 4, 5))
    encode(a, h5fl, "foo")

    five <- 5L
    sel <- Selector(file=h5fl, root="foo")[1:2, 2:3, 1:3, five]
    lin <- linearize(sel)
    res <- array(as.logical(lin), dim=dim(a))

    tar <- array(FALSE, dim=dim(a))
    tar[1:2, 2:3, 1:3, five] <- TRUE
    checkIdentical(tar, res)
}
##test_subset_variable_names()

## not really a test, just here to tickle more '...' args
test_linearize_4d <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    a <- array(1:120, c(2, 3, 4, 5))
    encode(a, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")[1:2, 2:3, 1:3, 5]
    lin <- linearize(sel)
    res <- array(as.logical(lin), dim=dim(a))

    tar <- array(FALSE, dim=dim(a))
    tar[1:2, 2:3, 1:3, 5] <- TRUE
    checkIdentical(tar, res)
}
##test_linearize_4d()

test_unnamed_matrix_materialize <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    m <- .unnamed_matrix()
    encode(m, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")[2, 2:3]

    res <- mat(sel)
    tar <- m[2, 2:3]
    ##print(res); print(tar)
    checkIdentical(tar, res)
}
##test_unnamed_matrix_materialize()

test_unnamed_array_materialize_drop <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    a <- .unnamed_array()
    encode(a, h5fl, "foo")

    selFALSE <- Selector(file=h5fl, root="foo")[1:2, 3, 2:4, drop=FALSE]
    resFALSE <- mat(selFALSE)
    tarFALSE <- a[1:2, 3, 2:4, drop=FALSE]
    checkIdentical(tarFALSE, resFALSE)

    selTRUE <- Selector(file=h5fl, root="foo")[1:2, 3, 2:4]
    resTRUE <- mat(selTRUE)
    tarTRUE <- a[1:2, 3, 2:4]
    checkIdentical(tarTRUE, resTRUE)
}
##test_unnamed_array_materialize_drop()

test_getdims <- function() {
    h5fl <- h5robj:::.create_temp_h5()

    ## one-dimensional
    vec <- 1L:13L
    h5write(vec, h5fl, "foo")
    dims <- h5robj:::getdims(h5fl, "foo")
    checkIdentical(13L, dims)
    
    ## two-dimensional
    m <- matrix(1:20, ncol=10)
    h5write(m, h5fl, "bar")
    dims <- h5robj::getdims(h5fl, "bar")
    checkIdentical(c(2L, 10L), dims)
}
##test_getdims()

test_unnamed_vector <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- 8:10
    h5robj::encode(vec, h5fl, "foo")

    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2:3]
    res <- mat(sel2)
    tar <- vec[2:3]
    ##print(res); print(tar)
    checkIdentical(tar, res)
}
##test_unnamed_vector()

## test_named_dimnamed_matrix <- function() {
##     h5fl <- h5robj:::.create_temp_h5()
##     m <- matrix(11:16, 2, 3, dimnames=list(letters[1:2], letters[24:26]))
##     names(m) <- paste0(letters[1:6], letters[1:6])
##     h5robj::encode(m, h5fl, "foo")
    
##     sel <- Selector(file=h5fl, root="foo")
##     sel2 <- sel[2, 2:3]
##     res <- mat(sel2)
##     tar <- m[2, 2:3]
##     ##print(res); print(tar)
##     checkIdentical(tar, res)
## }
## test_named_dimnamed_matrix()

## COMMON SUBSET PERMUTATIONS

test_common_subsets_repeated <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## repeated
    sel_repeat <- sel[c(1, 1)]
    res <- mat(sel_repeat)
    checkIdentical(vec[c(1, 1)], res)
}
##test_common_subsets_repeated()

test_common_subsets_discontiguous <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## discontiguous
    sel_discontig <- sel[c(1, 3)]
    res <- mat(sel_discontig)
    checkIdentical(vec[c(1, 3)], res)
}
##test_common_subsets_discontiguous()

test_common_subsets_arbitrary_order <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## arbitrary order
    sel_arb_ord <- sel[c(2, 1)]
    res <- mat(sel_arb_ord)
    checkIdentical(vec[c(2, 1)], res)
}
##test_common_subsets_arbitrary_order()

test_common_subsets_inverse_selection <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## inverse selection (dropping)
    sel_drop <- sel[-3]
    res <- mat(sel_drop)
    checkIdentical(vec[-3], res)    
}
##test_common_subsets_inverse_selection()

test_common_subsets_zero_included <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## including 0 among indices
    sel_zero_mix <- sel[c(0, 2)]
    res <- mat(sel_zero_mix)
    checkIdentical(vec[2], res)
}
##test_common_subsets_zero_included()

test_common_subsets_zero_only <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    sel <- Selector(file=h5fl, root="foo")

    ## 0 as only index
    sel_zero_alone <- sel[0]
    res <- mat(sel_zero_alone)
    checkIdentical(vec[0], res)
}
##test_common_subsets_zero_only()

test_iterative_subsetting <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- c(a=42L, b=39L, c=101L)
    h5robj::encode(vec, h5fl, "foo")
    
    sel <- Selector(file=h5fl, root="foo")
    sel2 <- sel[2:3][2]
    res <- mat(sel2)
    checkIdentical(vec[3], res)
}
##test_iterative_subsetting()

## test_recursive_structure <- function() {
##     h5fl <- h5robj:::.create_temp_h5()
##     vec <- list(10:15, 4)
##     encode(vec, h5fl, "bar")
##     dta <- new("DimTuple", min=1L, max=6L)
##     dtb <- new("DimTuple", min=1L, max=1L)
##     s <- new("Selector", file=h5fl, root="bar", dimLimits=list(dta, dtb),
##              dimSelection=vector('list', length(dt)))
## s2 <- s[5:6, 1]
## print(s2)
## }
