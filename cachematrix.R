## KG 22-MAR-2015
## makeCacheMatrix to cache the Inverse operation for increasing performance

makeCacheMatrix <- function(x = numeric()) {
        inverse <- NULL   ## initialize inverse value
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(mean) inverse <<- solve(x)   ## if necessary find inverse using solve function
        getInverse <- function() m
        matrix(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve to find the inverse using cacheed version, this approach will improve the performance

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
