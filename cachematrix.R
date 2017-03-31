## Week 3 - R Programming
## In this assignment, a pair of functions that cache the inverse of a matrix was written below.
## Caching the inverse of a matrix rather than computing it repeatedly is more efficient.


## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

## Test with a 4x4 matrix generated from rnorm(). Any invertibal square matrix can be used.
## cachesolve has to be used twice to check if data is cached.

yastest=matrix(rnorm(16),4,4)
cachedmatrix=makeCacheMatrix(yastest)
cacheSolve(cachedmatrix)
cacheSolve(cachedmatrix)

## Second test with a 3x3 matrix.

yastest=matrix(c(1,2,1,1,3,4,4,4,5),3,3)
cachedmatrix=makeCacheMatrix(yastest)
cacheSolve(cachedmatrix)
cacheSolve(cachedmatrix)