## The functions below are aimed to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Cache Matrix
    inv <- NULL
    
    ## GET & SET for Cache Matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## GET & SET for Cache Matrix Inverse
    getinverse <- function() inv
    setinverse <- function(inverse) inv <<- inverse
    
    ## Return List
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    # Inverse Cache Matrix
    if (!is.null(inv)) {
        message("Inverse Cache")
        return(inv)
    }
    
    # Calculate Inverse Matrix
    m <- x$get()
    
    # Return Inverse Matrix
    inv <- solve(m, ...)
    x$setinv(inv)
    return(inv)
}