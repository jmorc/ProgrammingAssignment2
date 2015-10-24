## functions makeCacheMatrix and cacheSolve are for creating and
## accessing a data structure that consists of a matrix and
##its cached inverse matrix

## makeCacheMatrix creates a list of getter and setter functions
## set(x) sets the matrix and sets the cached inverse to null
## get() returns the matrix x
## setinv(matrix_inverse) sets the cached matrix inverse to 
##    the argument matrix_inverse
## getinv() returns the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(matrix_inverse) inv <<- matrix_inverse
    getinv <- function() inv
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve(x, ...) takes as its first argument, x, the list
## structure returned by makeCacheMatrix.  The function assigns 
## the cached matrix inverse to the varible inv.  If there is
## a cached matrix inverse, it is returned. If there is
## no cached matrix inverse (inv is null), the inverse is calculated,
## stored in the input argument x, and returned.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
