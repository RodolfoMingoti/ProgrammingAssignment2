## With this functions you get the inverse of your matrixes and storing it in the cache so you can save some calculation time.
## it was really hard to understand and solve that lesson!



## This is the function that creates the getters and setters for later calculations.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the function that creates the inverse of a matrix and stores it in the cache to use it later in case the result was already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' whether it can take it from
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
