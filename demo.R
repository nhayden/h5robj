suppressMessages({library(IRanges); library(h5robj)})

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



## lists

## data.frame

## vectors
