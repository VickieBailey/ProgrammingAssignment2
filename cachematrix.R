## Vickie Bailey
## R Programming - Week 3 Assignment 2


## Part 1:
## This function creates a special "matrix" object that can cache its inverse.
## Assuming the matrix is square and invertible.
## Define x as an empty matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inv as an object in the makeCacheMatrix parent environment. 
    inv <- NULL
    
    ## Define the set() function.
    set <- function(y) {
        
        ## Assign values to x and inv in the parent environment with `<<-`.
        ## When x is reset, the cached value inside inv is set to NULL.
        x <<- y
        inv <<- NULL
    }
    
    ## Retrieve the value of x from parent environment of makeCacheMatrix.
    get <- function() x
    
    ## Define set for the inverse inv from the parent environment.
    ## Assigns the inverse to the parent environment.
    setinv <- function(solve) inv <<- solve
    
    ## Define which inv to get - from the parent environment.
    getinv <- function() inv
    
    ## Assign each function as an element in list().
    ## Return to parent environment.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function computes the inverse of the matrix from makeCacheMatrix.

cacheSolve <- function(x, ...) {
    
    ## Gets inv from cache in makeCacheMatrix
    inv <- x$getinv()
    
    ## If the inverse has already been calculated because x and inv are
    ##  not reset, get it from the cache instead of recalculating.
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## Otherwise calcuate the inverse.
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Then set the value of the new inverse in the cache in makeCacheMatrix.
    x$setinv(inv)
    
    ## Return a matrix that is the inverse of 'x'
    return(inv)
    
}
