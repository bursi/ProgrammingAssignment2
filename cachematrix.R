## these functions do the same as the example from the assignement with the mean replaced by the inverse.

## the function makeCacheMatrix takes a matrix as an argument and returns a list of functions to
## - set the matrix (allows to change the elements of the matrix and reset the inverse to NULL without re-defining the whole list)
## - get (i.e. return) the matrix
## - set (i.e. calculate) the inverse
## - get (i.e. return) the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the function cacheSolve takes a "CacheMatrix" as an argument and checks it's inverse is already stored in the CacheMatrix.
## If so, it returns the cached inverse. Otherwise, it calculates the inverse, stores it in the cache and returns it.

cacheSolve <- function(x, ...) 
{
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
