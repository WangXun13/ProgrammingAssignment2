## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() {
        x
    }
    list(set = set, get = get,
         setInverse = function(inverse) {
             cache <<- inverse
         },
         getInverse = function() {
             cache
         })
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("Getting cached data.")
        return(inverse)
    } else {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        return(inverse)
    }
}

