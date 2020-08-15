## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix:
## Matrix inversion is sometimes a costly computation and there may be a benefit of caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to cache and reused the result instead of recomputing it
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
  
}
