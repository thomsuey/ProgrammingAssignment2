## This script creates two function that when used in combination allow for the
## caching of an inverse matrix derived from a special matrix storage object

## This function creates an object that can store a matrix and
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set inverse to NULL so that is.null can be used in cacheSolve
    ## to determine whether to use the cache or not
    inverse <- NULL
    
    ## function to store the matrix passed into makeCacheMatrix
    setMatrix <- function(y)
    {
        ## redefine x in the parent scope
        x <<- y
        ## nullify inverse in the parent scope (not calculated yet on new x)
        inverse <<- NULL
    }
    
    ## function to fetch current x
    getMatrix <- function() x
    
    ## function to redefine inverse in the parent scope
    setInverse <- function(i) inverse <<- i
    
    ## function to fetch current inverse
    getInverse <- function() inverse
    
    ## return list of methods for setting/getting matrix and inverse
    ## all but setMatrix() used by cacheSolve to evaluate/store inverse
    list(
          setMatrix  = setMatrix
        , getMatrix  = getMatrix
        , setInverse = setInverse
        , getInverse = getInverse
    )
}


## This function computes if necessary or retrieves from cache the inverse of 
## the matrix stored using the function above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get inverse value stored in passed "matrix" list object (x)
    inverse <- x$getInverse()
    
    ## if a non-null inverse value exists, use it
    if(!is.null(inverse))
    {
        message("From cache...")
        return(inverse)
    }
    
    ## otherwise, retrieve the matrix, calculate the inverse, store it, and 
    ## then return it
    xMatrix <- x$getMatrix()
    inverse <- solve(xMatrix, ...)
    x$setInverse(inverse)
    inverse
}
