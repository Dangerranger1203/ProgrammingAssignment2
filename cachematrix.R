

## This function makes the list of functions to be used to get the inverse
## and storing the cached data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## the above set() sets the value of matrix using lexical scoping
## get() gets the matrix data
## setinverse() calculates the inverse
## getinverse() gets the invese
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this  function is to solve the inverse of matrix if it is not already available
## in the makeCacheMatrix()
## if the matrix is already available it gets the cached value of the inverse of
## the existing matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
