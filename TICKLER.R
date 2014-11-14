R
library(h5robj)
h5fl = h5robj:::.create_temp_h5()
vec = list(8:10)
attr(vec, "fun") = "tastic"
##vec = 8:10
encode(vec, h5fl, "foo")
asel = Selector(h5fl, "foo")
asel



## h55 = h5id(h5fl, "foo")
## h55
