h5robj
======
Disk-based representation of R objects with rectangular subsetting functionality.

Object types supported
----------------------
h5robj currently supports R objects of arbitrary types (including S3/S4), as long as they are composed of objects that are of the following types or are composed of or inherit from the following types:
* atomic vector types supported by [rhdf5](http://www.bioconductor.org/packages/release/bioc/html/rhdf5.html "Bioconductor rhdf5 package"); a notable missing type is `complex`.
* lists
* name / symbol objects (`SYMSXP`s in C)

Encode / decode entire objects
------------------------------
h5robj currently supports encoding and decoding entire arbitrary R objects, via `encode` and `decode`

Subsetting on-disk R objects
----------------------------
A major goal of h5robj is to support subsetting functionality for disk-based R objects, to enable processing of data too large to fit in memory. This is accomplished by creating lightweight "skeleton" objects of the underlying HDF5-based representations of encoded objects.
