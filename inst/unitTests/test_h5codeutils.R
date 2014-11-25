suppressMessages({
    library(h5robj)
    library(RUnit)
    library(IRanges)
})

.factor <- function() {
    vals <- c("d", "a", "a", "e", "e", "b", "a", "e", "c", "b")
    lvls <- c("a", "b", "c", "d", "e")
    factor(vals, lvls)
}

## verify that `\001NULL\001` is the representation used by
## methods:::.pseudoNULL for the pseudoNULL object; pseudoNULL is used
## as a placeholder symbol for *ORNULL slots in S4 objects
test_pseudoNULL_represenation <- function() {
    checkIdentical(h5robj:::.pseudoNULL, methods:::.pseudoNULL)
}
##test_pseudoNULL_represenation()

test_h5ls_immediate_descendants <- function() {
    ff <- .factor()

    h5fl <- h5robj:::.create_temp_h5()
    top_name <- "foo"

    encode(ff, h5fl, top_name)
    H5close()

    descs <- h5robj:::h5ls_immediate_descendants(h5fl, "foo/attrs")
    tar_descs <- c("foo/attrs/class", "foo/attrs/levels")
    checkIdentical(tar_descs, descs)
}
test_h5ls_immediate_descendants()

test_has_groups <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    foo <- integer()

    encode(foo, h5fl, "foo")
    checkTrue(!has_attrs_group(h5fl, "foo"))
    checkTrue(!has_data_group(h5fl, "foo"))
    checkIdentical(attrs_group(h5fl, "foo", checked=TRUE), NULL)
    checkIdentical(attrs_group(h5fl, "foo"), "foo/attrs")
    checkIdentical(data_group(h5fl, "foo", checked=TRUE), NULL)
    checkIdentical(data_group(h5fl, "foo"), "foo/data")
    checkIdentical(single_data_path(h5fl, "foo", checked=TRUE), NULL)
    checkIdentical(single_data_path(h5fl, "foo"), "foo/data/data")
    checkTrue(is_empty_obj(h5fl, "foo"))
    
}

test_has_attr_x <- function() {
    h5fl <- h5robj:::.create_temp_h5()
    vec <- 8:10
    attr(vec, "class") <- "funtastic"

    encode(vec, h5fl, "foo")
    h5ident <- h5id(h5fl, "foo")
    checkTrue(has_attr_x(h5ident, "class"))
    checkTrue(h5robj:::.has_attr_x(h5fl, "foo", "class"))
    checkTrue( !has_attr_x(h5ident, "bogus") )
    checkTrue( !h5robj:::.has_attr_x(h5fl, "foo", "bogus"))
}
