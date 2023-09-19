## Programming Assignment 2: Lexical Scoping


## makeCacheMatrix creates the cache environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
                }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. It first looks through cache 
## memory for the solution. If not found in cache memory, it will add it to
## the cache memory for next time.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) #returns inverse if it is in cache memory
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)  #if it wasn't in cache before, solves and saves to cache
        inv
}