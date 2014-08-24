## MakeCacheMatrix and cacheSolve are functions that allow the inverse of a 
## matrix to be stored in cache memory once calculated. The inverse is 
## calculated by cacheSolve only if there is no cached value.

## MakeCacheMatrix
## This function creates an object that contains a matrix and a cached value
## of its inverse (initialized as a NULL value until calculated for the first
## time).
## Methods are also defined to set the matrix value (resetting the cached
## inverse), get the matrix value and set or get the cached inverse value.
## Return is a list of functions, i.e. the methods defined for this object.

makeCacheMatrix <- function(x = matrix()) {  
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
## This function first gets the cached inverse value and immediately returns
## that value if it is not NULL.
## Otherwise, the function reads the matrix and computes the inverse using
## the 'solve' function and then caches it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)){
        message("getting cached value")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
}
