## This function takes as input a matrix.
## It initializes the inverse of that matrix (i) to NULL
## and then creates a set of nested functions
## which set and get the value of the matrix, 
## and set and get the value of its inverse, respectively.
## It returns these functions as a list

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <<- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a cacheMatrix object as defined by the function makeCacheMatrix.
## It uses the getinverse function of that object to find retrieve the inverse of the matrix.
## If this is not null, it has already been calcuated and cached, so an appropriate message is output.
## Otherwise, the data for the matrix is retrieved using the get function of the object,
## and the inverse is calculated using the solve function.
## The inverse is then stored in the cached using the setinverse function of the cacheMatrix object
## and the inverse of the original matrix is output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
