## Introduction to R - Programming Assignment 2

## Creates a special matrix object that can cache it's reverse

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
