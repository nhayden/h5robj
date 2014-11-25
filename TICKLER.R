R
library(h5robj)
h5fl = h5robj:::.create_temp_h5()
vec = list(8:10)
attr(vec, "fun") = "tastic"
##vec = 8:10
encode(vec, h5fl, "foo")
asel = Selector(h5fl, "foo")
asel


library(h5robj)
h5fl = h5robj:::.create_temp_h5()
df = data.frame(a=1:7, b=letters[12:18])
encode(df, h5fl, "foo")
sel = Selector(h5fl, "foo")

## Example of contains
track <- setClass("track", slots=c(x="numeric", y="numeric"))
trackCurve <- setClass("trackCurve", slots=c(smooth="numeric"), contains="track")

## This is the inverse!!
.A <- setClass("A", slots=c(x="numeric"))
.B <- setClass("B", slots=c(y="numeric", b="A", bget="function"))
B <- function(y, b) { .B(y=y, b=b, bget=function(thing) thing@x)}
Bget <- function(whoa) { whoa@bget(whoa@b) }
anA <- .A(x=15)
aB <- B(13, anA)
Bget(aB)
aB@b@x <- 16
Bget(aB)

.A <- setClass("A", slots=c(x="numeric", y="function"))
A <- function(x=42, y=function(fancy) function(i) slot(fancy, i)) .A(x=x, y=y)
.B <- setClass("B", slots=c(z="numeric", foo="A"))
B <- function(z, foo=A()) {
    theB <- .B(z=z, foo=foo)
    theB
}
doit <- function(aB, slotname) {
    aB@foo@y(aB)(slotname)
}
theB <- B(13, A())
doit(theB, "z")
theB@z = 26
extractA <- function(aB) {
    inner <- aB@foo
    inner@y <- inner@y(aB)
    inner
}
extracted <- extractA(theB)
extracted@y("z")
