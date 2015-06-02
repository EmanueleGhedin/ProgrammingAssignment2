## These two functions calculate and return the inverse
## of a given invertible matrix. If the inverse is already known
## and stored in the cache, it can be recovered without further
## computations (cashed). If is not known, this is calculated and stored
## in the cache for future faster computations.

## The first function creates the matrix (as list of functions).

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
        
}

## The second function cashes the inverse of a given matrix (constructed
## with the function above), or calculates it and stores it in the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
