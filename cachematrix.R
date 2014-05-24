## Below are two functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied in the function argument, is always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.
## The matrix object must be a square matrix.

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

## This cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()             #query the x's cache
    if(!is.null(m)) {            #if cache exists
        message("getting cached data")
        return(m)           # return the cache
    }
    data <- x$get()             #if there is no cache
    m <- solve(data, ...)        # compute the inverse of the matrix
    x$setInverse(m)                # save the result to x's cache
    m                           #return the result
}