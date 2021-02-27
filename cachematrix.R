## The following pair of functions cache the inverse of a matrix

## Assumption: The matrix supplied is always invertible

## The function "makeCacheMatrix" creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMXinverse <- function(solve) m <<- solve
  getMXinverse <- function() m
  list(set = set, get = get,
       setMXinverse = setMXinverse,
       getMXinverse = getMXinverse)
}


## The function "cacheSolve" computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getMXinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setMXinverse(m)
  m
  
}
