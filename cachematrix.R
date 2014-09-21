## Thes functions create a matrix and its inverse. The fist function
## creates a matrix and formulas to access it and its inverse.
## The second function calculates and caches the inverse.

## makeCacheMatrix caches a matrix specified in the argument of the
##functions. It also allows to call formulas to change the matrix or
## access it as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  create <- function(y) {
    x <<- y
    i <<- NULL
  }
  show <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list (create = create, show = show,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix and stores it in the 
## cache. If the inverse of the matrix was previously stored it accesses
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$show()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
