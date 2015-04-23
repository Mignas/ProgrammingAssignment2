## These functions are used to create a matrix and to calculate its inverse.
## To avoid unnecessary recomputation, the inverse is cached, and retrieved 
## if previously calculated.

## This function creates a list of functions for setting and getting the
## values of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
            x <<- y
     		inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
      	setInverse = setInverse,
      	getInverse = getInverse)
}

## This function calculates the inverse of a matrix and returns the inverse.
## If the inverse has been calculated already, it is not recalculated, but 
## retrieved from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
      if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
