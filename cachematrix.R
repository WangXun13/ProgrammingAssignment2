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

