## In this assignment there are two functions called 'makeCacheMatrix' and 'CacheSolve'.
## The aim of these functions is to cache the inverse of an invertible matrix for easy and quick retrieval.
##This results in a comparatively cost-effective computation.

## This function aims to get a value for the matrix (assumes invertible), and cache its inverse. 

makeCacheMatrix <- function(x = matrix()) { #parent function
        inv <- NULL
        set <- function(y) {
                x <<- y        # <<- enables me to manage variables at different levels and not just within this function.
                inv <<- NULL
                
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function aims to compute the inverse of the given matrix. It checks whether the inverse has been calculated before. 
##If the inverse has been calculated before and the matrix has not changed, it does not re-compute it and retrieves from cache. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {  #checks if inverse calculated before
                message("getting chached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)    #solve computes inverse of a matrix.
        x$setinv(inv)
        return(inv)
}
