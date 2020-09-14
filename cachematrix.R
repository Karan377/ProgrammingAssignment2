## Put comments here that give an overall description of what your
## functions do
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# These are functions that cache the inverse of a matrix.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  f<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setinverse(f)
  inv
}