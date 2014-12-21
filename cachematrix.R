## The functions in this file caches the inverse of a matrix
## so that the inverse for a given matrix is caculated only
## once and subsequent calls to inverse the matrix returns
## the cached result.

## This function returns a list which is a simple
## wrapper around the matrix. This list contains methods
## to set and get the matrix and its inverse and serves
## as a cache for the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix
## from cache if available, otherwise it caculates
## the inverse and stores it in the cache before returning
## the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
