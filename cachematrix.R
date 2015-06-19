## Put comments here that give an overall description of what your
## functions do

## Creates a Matrix

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
              x <<- y
              m <<- NULL
              }              
              
              get <- function() x
              setinv <- function(inv) m <<- inv
              getinv <- function() m
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}

## The cacheSolve function checks for the inverse of a matrix 
## in the cache and if not found it then goes on and calculates
## the inverse of the given matrix

cacheSolve <- function(x, ...) {
              m <- x$getinv()
              if(!is.null(m)) {
              message("getting cached data")
              return(m)
              }
              data <- x$get()
              m <- solve(data, ...)
              x$setinv(m)
              m 
        ## Return a matrix that is the inverse of 'x'
}
