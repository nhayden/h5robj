suppressMessages({library(IRanges); library(h5robj)})

## atomic vectors, and arbitrary attributes on any object type
h5fl <- h5robj:::.create_temp_h5()
vec <- c(a=8, b=9, x=10)
attr(vec, "fun") <- c("ctional", "tastic", "damental")
encode(vec, h5fl, "foo")
sel <- Selector(h5fl, "foo")
sel@h5attrs@selectors["fun"] = sel@h5attrs@selectors[["fun"]][2:3]
vec2 <- vec
attr(vec2, "fun") = attr(vec2, "fun")[2:3]
identical(vec2, mat(sel))

## N-dimensional arrays / matrices
a <- array(1:24, c(2, 3, 4))
sel <- encode(a) ## by default, encode returns a selector
res <- mat(sel[2, c(1, 3), c(1, 2, 4)])
tar <- a[2, c(1, 3), c(1, 2, 4)]
identical(res, tar)
res

## lists
h5fl <- h5robj:::.create_temp_h5()
l <- list(a=6L, fun=40:42, tastic=letters[15:18])
encode(l, h5fl, "foo")
sel <- Selector(h5fl, "foo")
mat(sel[c(1, 3)])

## data.frame
h5fl <- h5robj:::.create_temp_h5()
data(mtcars)
encode(mtcars, h5fl, "foo")
sel <- Selector(h5fl, "foo")
identical(mtcars, mat(sel))
identical(mtcars[seq(1, nrow(mtcars), 3), c(1, 3, 8)],
          mat(sel[seq(1, nrow(mtcars), 3), c(1, 3, 8)]))

## S4 example with parallel slots
ir <- IRanges(Rle(1:30) %% 5 <= 2)
h5fl <- h5robj:::.create_temp_h5()
encode(ir, h5fl, "foo")

sel <- Selector(h5fl, "foo")
class(sel)
decoded_ir <- mat(sel)
sel2 <- sel
sel2@h5attrs@selectors["start"] = sel2@h5attrs@selectors[["start"]][c(3, 5, 6)]
mat(sel2)

IRanges_grouped_slots <- c("start", "width")##, "NAMES", "elementMetadata")
subsetIRanges <- function(x, i) {
    irattrs <- x@h5attrs@selectors
    irattr_names <- names(irattrs)
    grouped <- which(irattr_names %in% IRanges_grouped_slots)
    for(elt in grouped) {
        irattrs[elt] <- irattrs[[elt]][i]
    }
    initialize(x, h5attrs=ListLikeSelector(selectors=irattrs))
}
sel3 <- subsetIRanges(sel, c(3, 5, 6))
identical(ir[c(3, 5, 6)], mat(sel3))
