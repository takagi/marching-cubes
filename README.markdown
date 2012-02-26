# Marching-Cubes

A marching cubes implementation based on Paul Bourke's.

## Usage

**Syntax:**

**marching-cubes** *density-function* *min-position* *max-position* *delta* *isolevel* => *triangles*

**Arguments and Values:**

*density-function* --- a function that takes a position as its 3 elements and returns the density at the point

*min-position* --- a vec3 value that specifies the minimum point of grid to be processed

*max-position* --- a vec3 value that specifies the maximum point of grid to be processed

*delta* --- a scalar vlaue that specifies the cell size of grid to be processed

*isolevel* --- a scalar value that specifies the threshold of mesh construction

*triangles* --- a list of triangles

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the LLGPL License.

