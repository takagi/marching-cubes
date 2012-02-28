# Marching-Cubes

A marching cubes algorithm implementation in Common Lisp based on Paul Bourke's (http://paulbourke.net/geometry/polygonise/)

## Usage

**Syntax:**

**marching-cubes** *density-function* *min-position* *max-position* *delta* *isolevel* => *triangles*

**marching-cubes-smooth** *density-function* *normal-function* *min-position* *max-position* *delta* *isolevel* => *smooth-triangles*

**Arguments and Values:**

*density-function* --- a function that takes a position as its three elements and returns density at the point

*normal-function* --- a function that takes a position as its three elements and returns a normal vector at the point

*min-position* --- a vec3 value that specifies the minimum point of grid to be processed

*max-position* --- a vec3 value that specifies the maximum point of grid to be processed

*delta* --- a scalar vlaue that specifies the cell size of grid to be processed

*isolevel* --- a scalar value that specifies the threshold of mesh construction

*triangles* --- a list of triangles

*smooth-triangles* --- a list of smooth triangles

## Example

See example/ directory.

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the LLGPL License.

