## Make matrix inversion faster by caching a matrix's inverse.

## makeCacheMatrix is a set of functions that sets and retrieves a matrix and its inverse.
## Returns a list of these four functions.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # sets and gets matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function()
        { x }
    
    # sets and gets inverse matrix
    setInverse <- function(inv)
        { m <<- inv }
    getInverse <- function() 
        { inverse }
    
    #return list of 4 functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    # Retrieve x's inverse from makeCache Matrix. If it exists, exit the function and return retrieved inverse
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # If retrieved inverse is null, retrieve matrix, solve for inverse, and cache this inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
