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
A major goal of h5robj is to extend the functionality of the rhdf5 package to enable processing of data too large to fit in memory, but do it in a transparent way. This is accomplished by creating lightweight "skeleton" objects of the underlying HDF5-based representations of encoded objects. From there, users can apply subset operations to the skeleton objects. The selection choices are recorded, but no actual data is read from HDF5 until a skeleton object is "materialized." When materializing, the selection choices are consulted so only the chosen subset of the data is read from the HDF5 file.

TODO: Examples of how-to. For now see top-level file [demo.R](demo.R)

Details of HDF5 representation
------------------------------
h5robj encodes R objects in a way that reflects (but not exactly mimics) the underlying R representation of objects. TODO: more details.
