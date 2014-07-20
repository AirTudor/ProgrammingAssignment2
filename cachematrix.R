##
## Description: Functions used to work with matrices for which the inverse is 
##              cached, given that computing the inverse of a matrix is a 
##              computationally expensive operation
## Author:      Tudor
## Date:        07/20/2014
##

##
## Summary: Constructs a special object holding the operations required
##          to work with a matrix where its inverse is cached
## Input:   Matrix for which we want to cache the inverse
## Output:  List of functions containing:
##          - get - Gets the matrix stored in the object
##          - set - Sets the matrix stored in the object
##          - getInverse - Gets the cached inverse of the matrix, if one was 
##                         set, and NULL otherwise
##          - setInverse - Sets the cached inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    
    if (is.null(x)) {
        stop("Input matrix cannot be null")
    }
    
    # Start with a NULL inverse, until one is cached
    inv <- NULL
    
    # Sets the matrix that we're working on and resets the cached value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Gets the matrix that we're working on
    get <- function() x
    
    # Caches the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Gets the cached inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of the operations defined above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##
## Summary: Obtains the inverse of a matrix
## Input:   List of operations obtained by calling makeCacheMatrix  
## Output:  Inverse of the matrix
##
cacheSolve <- function(x, ...) {
    
    if (is.null(x)) {
        stop("Input list of functions cannot be null")
    }
    
    # Attempt to get the cached inverse and return if it has been computed   
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    
    # Failed to get the inverse from the cache so compute one, cache it 
    # and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv
}
