
## Creates a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    m <<- NULL
    x <<- y
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list( setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse, getinverse = getinverse)
}


## Calculates inverse of the matrix created in makeCacheMatrix unless it
#has been cahced, in which case it returns the cached inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
