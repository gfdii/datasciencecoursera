## These functions are interrelated, the first will allow the second to calculate and check for storage
## in the global environment

## mCM pulls together a list of functions that allow caching of a matrix

makeCacheMatrix <- function(A = matrix()) {
      m <- NULL
      set <- function(y) {
            A <<- y
            m <<- NULL
      }
      get <- function() A
      inverse <- function(solve) m <<- inverse
      getinverse <- function() m
      list(get = get, inverse = inverse, getinverse = getinverse)
}

## First checks for 

cachesolve <- function(A, ...) {
      m <- A$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- A$get()
      m <- solve(data, ...)
      A$inverse(m)
      m
}