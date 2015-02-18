## The following two functions will enable the user to cache the inverse of a matrix.
## This caching will save the user from (unnecessary) repeated time-consuming
## computations, in this case from computing the inverse of a matrix

## The function makeCacheMatrix creates a special matrix object that allows to set and
## get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL # Initialize the inverse of the input matrix
    
    set <- function(y) { 
        x <<- y # Set a new input matrix 
        inv <<- NULL # If a new input matrix is set, re-initialize the inverse
    }
    
    get <- function() x # Get the cached input matrix
    
    setinverse <- function(inverse) inv <<- inverse # Set the inverse of the input matrix
    
    getinverse <- function() inv # Get the (cached) inverse of the input matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The function cacheSolve returns the inverse of a matrix. The input has to be a
## special matrix object that has been created with makeCacheMatrix. If the inverse
## has already been cached the function returns the cached value and avoids the
## repeated computation of the inverse.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse() # Get the (cached) inverse of the input matrix
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) # Return the cached inverse of the input matrix
    }
    
    # The next commands will only be executed if the inverse has not been cached
    
    data <- x$get() # Get the input matrix
    inv <- solve(data, ...) # Compute the inverse of the input matrix
    x$setinverse(inv) # Cache the inverse of the input matrix
    
    inv # Return the inverse of the input matrix
    
}
