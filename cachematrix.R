## This script creates two function that when used in combination allow for the
## caching of an inverse matrix derived from a special matrix storage object

## This function creates an object that can store a matrix and
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setMatrix <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
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
    inverse <- x$getInverse()
    if(!is.null(inverse))
    {
        message("From cache...")
        return(inverse)
    }
    xMatrix <- x$getMatrix()
    inverse <- solve(xMatrix, ...)
    x$setInverse(inverse)
    inverse
}
