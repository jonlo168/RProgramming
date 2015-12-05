##
## This file contains the functions used to create a special "matrix" object 
## where its inverse is computed and cached for future use. This provides
## computational benefit in situation where a matrix inverse needs to be 
## computed repeatedly (e.g. in a loop). 
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## Input 
    ##      'x' is the matrix used to initialise this special "CacheMatrix"
    ##
    ## Output
    ##      Returns a list containing below functions:
    ##          
    ##      'set(y)' sets the matrix to input y
    ##
    ##      'get()' returns the stored matrix
    ##
    ##      'setmean(y)' sets the matrix inverse to input y
    ##
    ##      'getmean() returns the stored matrix inverse
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    
    ## Input 
    ##      'x' is the input matrix to have its inverse computed
    ##
    ##      ... are the optional inputs to the function "solve"
    ##
    ## Output
    ##      Returns the inverse of input matrix 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    
    inv
}
