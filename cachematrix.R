## Functions for computing the inverse of a square matrix and caching the
## object as it is a time-consuming computation. Next time the inverse 
## of the same matrix is requested, cached data is got

## Constructor for storing and caching a square matrix object and its 
## inverse, and defining the basic get/set functions of the object

makeCacheMatrix <- function(x = matrix()) {
	
	## reset inverse
    inv <- NULL
    
    ## set the matrix stored and reset its inverse
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    
    ## get the stored matrix
    get <- function() x
    
    ## set / get the inverse of the stored matrix
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    ## list the basic get/set functions for the matrix object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
		 
}


## Function for computing the inverse a stored matrix (new matrix) 
## or getting the inverse from the cache (existing matrix)

cacheSolve <- function(x, ...) {

    ## get the cached inverse (if it exists)
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inversion")
        return(inv)
    }
    
    ## if the cached inverse doesn't exists, getting the stored matrix and
    ## computing, caching and returning the inverse
    data <- x$get()
    ## As PA2 stated, it is assumed that it is a square matrix and
    ## its inverse is computable
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
	
}
