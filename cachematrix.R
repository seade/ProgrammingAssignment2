## Provided are a pair of functions that cache the potentially expensive inverse
## of a matrix operation.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Somewhere to store the computed inverse
        i <- NULL
        # The matrix is stored in x, doing do invalidates the cached i
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # Retrieve the matrix
        get <- function() x
        # Store the inverse matrix
        setinv <- function(inv) i <<- inv
        # Retrieve the inverse matrix
        getinv <- function() i
        # Response needs to provide details of available methods
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # Retrieve the cached value
        i <- x$getinv()
        if(!is.null(i)) {
                # cached value was found, so return that
                message("getting cached data")
                return(i)
        }
        # retrieve the matix so inverse can be computed
        data <- x$get()
        # compute the inverse
        i <- solve(data)
        # cache the computed invoice
        x$setinv(i)
        # return the computed inverse (next time should be faster)
        i
}
