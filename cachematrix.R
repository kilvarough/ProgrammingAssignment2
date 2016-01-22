## These two functions work together to calculate and store the inverse of a matrix in cache.

## There are four parts to this function:
## set() - simply saves the input matrix and saves NULL to the inverse matrix
## get() - returns the original matrix
## setMatrixInverse() - stores the inverse of the original matrix
## getMatrixInverse() - retrieves the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(matrixInverse) mi <<- matrixInverse
    getMatrixInverse <- function() mi
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## Returns the inverse of a matrix by either finding it in cache or calculating it an then storing it in cache for future use.

cacheSolve <- function(x, ...) {
    mi <- x$getMatrixInverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setMatrixInverse(mi)
    mi
}
