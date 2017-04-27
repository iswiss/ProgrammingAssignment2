## Put comments here that give an overall description of what your
## functions do
## The idea is to use a matrix inverse function already available in R so I don't have to code one myself
## I import the MASS package which gives a matrix inverse function (ginv()) as described here
## http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
require(MASS)

## First function will take a matrix and store it in data
## It also provides functions to store and retrieve the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second function will take the cached matrix object
## It will either take the cached inverse or calculate the inverse using ginv()

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinverse(m)
        m
}
