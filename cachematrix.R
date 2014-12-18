## The function makeCacheMatrix constructs a "special" matrix-like object.
## If the matrix has been inverted (solved), the inverted matrix is stored, for 
## easy retrival later.

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


## The function cacheSolve retrieves the inverse of the matrix constructed with makeCacheMatrix
## If the inverse has been calculated, the inverse is simply retrieved, and returned by
## the function. If the inverse has not been calculated already, the function triggers 
## the calculation of the inverse, and stores the inverse in the cache, before returning the
## inverse matrix. 

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


