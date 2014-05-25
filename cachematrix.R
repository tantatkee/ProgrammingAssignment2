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
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
