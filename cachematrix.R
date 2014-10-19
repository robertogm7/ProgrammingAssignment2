## This file contains a function to create a matrix in cache (makeCacheMatrix)
## and a function that gets the inverse matrix from cache when the matrix
## hasn't changed (cacheSolve)

## This function creates a special "matrix" object that has its inverse in cache
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with 
## the above function (makeCacheMatrix). First, checks if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if (!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
