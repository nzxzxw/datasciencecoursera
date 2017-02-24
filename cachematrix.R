## Together, these functions are designed to save processing time by 
## committing the output of a matrix inversion function to the cache.
## These functions create a special matrix and cache its inverse.

## The following function creates a special matrix. It sets the value of the 
## matrix, gets the value of the matrix, sets the value of its inverse, 
## and gets the value of its inverse.

makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## If the matrix returned by the makeCacheMatrix function has already been 
## solved, the following function retrieves its value from the cache. If not, 
## the function calculates and returns its inverse.
 
cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}