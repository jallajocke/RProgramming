## Functions for creating and getting the inverse of a matrix, caching
## the value of inverse once calculated, which reduces computations for
## every succeeding retrieval of the inverse.


## Creates a matrix from 'x' with a cached inverse. Returns a list 
## of get/set functions for the matrix and its inverse:
# x$set - sets the value of the matrix
# x$get - gets the value of the matrix
# x$setInverse - sets the value of the inverse
# x$getInverse - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x', assuming it has one.
## Takes advantage of the cached ability of x.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(is.null(inv)) {
    inv <- solve(x$get())
    x$setInverse(inv)
  }
  inv
}
