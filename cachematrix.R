## The two functions can cache the inverse of a matrix
## It is useful because computing of inverse of matrixcan be very time-consuming

## The function makeCacheMatrix creates a matix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
         s <- NULL
         set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 }


## This function computes the inverse of the matrix object returned by function makeCacheMatrix
## and checks if the inverse has not yet been calculated. 

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
	  x$setinverse(s)
        s
}