## This file defines a pair of functions that create a special matrix
## object capable of caching its own inverse, so the (potentially
## costly) inversion is computed only once and reused thereafter.


## makeCacheMatrix: creates a special "matrix" object that can cache
## its inverse. It returns a list of four functions:
##   set         - store a new matrix (and clear any cached inverse)
##   get         - retrieve the stored matrix
##   setInverse  - cache the computed inverse
##   getInverse  - retrieve the cached inverse (NULL if not yet cached)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                       # holds the cached inverse

        set <- function(y) {
                x   <<- y                 # store the new matrix
                inv <<- NULL              # invalidate any cached inverse
        }

        get <- function() x

        setInverse <- function(inverse) inv <<- inverse

        getInverse <- function() inv

        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), it retrieves the inverse from the cache and
## skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()

        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv  <- solve(data, ...)          # assumes the matrix is invertible
        x$setInverse(inv)
        inv
}
