## assign makeCacheMatrix with its matrix arg to a variable
## applies solve function to get and cache matrix inverse

## input is an invertible matrix, output is a named list of four functions
## set sets the value of the matrix
## get gets the value of the matrix
## setinverse sets the inverse of the matrix
## getinverse gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## checks if inv is null--inverse not yet solved
## if null, it solves, if not null it gets from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
