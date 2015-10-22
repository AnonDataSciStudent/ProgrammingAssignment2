## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This file provides a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # create an empty variable to store the value of the inverse
        inverse <- NULL

        # create a function to set the value of the matrix
        set <- function(y) {
                x <<- y # stores the new value of the matrix
                inverse <<- NULL # clears the old value of the inverse
        }

        # create a function to get the value of the matrix
        get <- function() x # returns the value of the matrix

        # create a function to set the value of the inverse
        setinverse <- function(z) {
                inverse <<- z # stores the new value of the inverse
        }

        # create a function to get the value of the inverse
        getinverse <- function() inverse # returns the value of the inverse

        # return a list of these four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix, making use of its cache.

cacheSolve <- function(x, ...) {
        # get the stored value of the inverse of x
        inverse <- x$getinverse()

        # determine if this value is not empty, i.e. if the inverse was cached
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse) # in that case, return the cached value
        }

        # otherwise, get the stored value of the matrix x
        data <- x$get()
        # calculate its inverse
        inverse <- solve(data, ...)
        # and store/cache this value
        x$setinverse(inverse)
        # finally, return this value
        inverse
}
