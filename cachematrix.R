## The following functions are used to callculate the inverse of a square
## matrix. To safe computing time the functions are set up to first
## check if the inverse of the matris has already been computed


## This functions creates a list containing functions
## and caches the inverse

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

## This function cecks for cached data and if neccessary calculates
## the inverse of the matrix
## also returns the value of the inversed matrix

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
}