## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix by Karthiganesh for evaluation

makeCacheMatrix <- function(x = martix) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(mean) inverse <<- solve(x)
        getInverse <- function() m
        matrix(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve by Karthiganesh for evaluation

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setInverse(m)
        m
}
