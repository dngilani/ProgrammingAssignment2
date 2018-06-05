## Used together, these functions take a matrix, return it's inverse, and add this inverse to the cache.
## In the future, when the user requests to find the inverse of this same matrix, this function will pull the
## answer from cache, instead of computing the inverse again. This will save time and processing power.

## This makeCacheMatrix function creates the matrix in hand and saves it to the cache.

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This cacheSolve function computes the inverse of a matrix created from the makeCacheMatrix function, and then 
## stores the inverse in cache.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
