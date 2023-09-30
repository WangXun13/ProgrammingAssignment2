# Function to create a special "matrix" object that can cache its inverse
## set- set the value of the matrix,
## get- get the value of the matrix
## setInverse- set the value of the inverse
## getInverse- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() {
        x
    }
    setInverse = function(solve) {
        cache <<- solve
    }
    getInverse = function() {
        cache
    }
    list(set = set, get = get,
         setInverse=setInverse, getInverse=getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
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

# Example usage:
# Create a cacheMatrix object with a matrix
test <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2,2))
test$get()
cacheSolve(test)
