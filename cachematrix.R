## Put comments here that give an overall description of what your
## functions do
# Theses functions allows data that is presented as an invertible matrix to be 
# cached, both the non-inverted and inverted matrix.

## Write a short comment describing this function
# Sets and gets the matrix and the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
# Provides the cached inverse of the given matrix, but if the given matrix 
# inverse value equals null then this function calculates the inverse, then 
# caches the inverse and finally returns the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("Getting cached inversed matrix")
    return(m)
  }
  data <- x$get()
  class(data)
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
