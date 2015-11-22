## Caching the Inverse of a Matrix. Matrix inversion is usually a costly
## computation and there may be some benefits to caching the inverse of a
## matrix rather than compute it repeatedly. The followings functions
## calculates the inverse of the "matrix" and caches its inverse.

## The following function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## The following function returns a matrix that is the inverse of parameter 'x'
## If the inverse has already been calculated then it should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
