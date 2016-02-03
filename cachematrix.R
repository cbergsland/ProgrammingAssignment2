## Two functions that together solve and cache the inverse of a matrix.

## First function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv 
  getinverse <- function () m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function computes the inverse of the matrix returned by
## the first function "makeCacheMatrix". If it has already been
## calculated, the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
