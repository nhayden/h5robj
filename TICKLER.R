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
