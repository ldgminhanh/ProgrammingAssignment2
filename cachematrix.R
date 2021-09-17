## These two functions use for caching the inverse of a matrix rather than compute it repeatedly.
## This function will give an output of a matrix and inverse matrix if it has in cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## If the 'makeCachematrix' gives getinv a NULL, we will use this second function to inverse the input matrix and return the output. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
