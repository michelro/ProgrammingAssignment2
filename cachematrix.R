## This file contains two functions, makeCacheMatrix and cacheSolve,
## that creates a special matrix object and compute the matrix inverse,
## respectively.

## Creates a special matrix object that can cache its inverse.
## Input:
##              x: Matrix
## Output: Special matrix, containig the functions:]
##              get(): returns the original matrix
##              set(y): sets a new matrix
##              setInverse(): sets a value for the matrix inverse
##              getInverse(): returns the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(y) inverse <<- y
        
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes and return the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
## Input: 
##              x: special matrix, returned by makeCacheMatrix function
##              ...: additional parameters, submited to the solve() function
## Output: x inversion
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        if (!is.null(inverse)) {
                message("getting cached value")
                inverse
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        
        inverse
}
