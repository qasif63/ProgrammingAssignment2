## --------------- INTRODUCTION ---------------
## Put comments here that give an overall description of what your
## functions do
## The two functions makeCacheMatrix() and cacheSolve() are written
## to create a special matrix object (which is actually a list) and
## calculating its inverse matrix. A special <<- operator is used 
## in the makeCacheMatrix() function to store objects in its cache.

## --------------- makeCacheMatrix ---------------
## This function creates a special "matrix" object which can cache 
## its inverse for the square matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## --------------- cacheSolve ---------------
## This function calculates the inverse of the square matrix returned
## by makeCacheMatrix. If the cache has already the inverse of the 
## matrix, then cacheSolve should be able to retrive the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
