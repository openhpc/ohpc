This program constructs a sparse matrix and performs a (parallel)
sparse matrix-vector multiplication. The sparse matrix is built as
follows. The standard star-shaped discretization stencil with a
user-specified radius is applied to a structured 2-dimensional
grid. Example of a stencil with radius r=2:

                              0
                              |
                              0
                              |
                        0--0--0--0--0
                              |
                              0
                              |
                              0

Here, the `0' symbol signifies inclusion in the stencil. A square grid
with linear dimension (2^n) has 2^(2n) = 4^n points. The resulting
matrix has (4^n) rows and (4^n) columns, for a total of (16^n)
elements. The user specifies n. The stencil is applied in a periodic
fashion, i.e. it wraps around the edges of the grid.

If the scramble flag is unset in the Makefile, the discretization
stencil results in a regularly banded sparse matrix, which can be
stored efficiently in vectors, in principle. If the scramble flag is
maintained, the columns of the matrix are permuted, resulting in a
general irregular sparse matrix, but with a known number of nonzeroes
per row (4r+1). We use Compressed Row Storage for accessing the matrix
elements, even in the case of an unset scramble flag. Numerical values
of matrix elements are chosen judiciously to make verification
easy. They do not correspond to any realistic discretization of a
continuum problem.
