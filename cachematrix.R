## This package implements a matrix object that manages cached matrix inverse.

## makeCacheMatrix returns a cacheMatrix object - an object that stores a
## matrix, but also exports the following methods for accessing and
## manipulating the object:
## (1) set: set the content of the stored matrix.
## (2) get: get the content of the stored matrix.
## (3) setinverse: memorize the given matrix as the inverse of the matrix
##     object.
## (4) getinverse: retrieve the memorized matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    ## Since we are setting a new value, we need to invalidate the cached
    ## inverse.
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the matrix inverse of the stored matrix in the given
## cacheMatrix.
cacheSolve <- function(x, ...) {
  ## First retrieve the cached inverse.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## If the cached inverse is not null, we can simply return it.
    message("getting cached data")
    return(inverse)
  }
  ## Otherwise, we re-compute the inverse, and cache it by using the
  ## setinverse method.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
