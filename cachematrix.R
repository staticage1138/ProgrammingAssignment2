## makeCacheMatrix creates a list of 4 functions that are used to 
## initialize a set of 4 functions as list accesible by the global
## environment.  

## cacheSolve checks for a prior calculation of the inverse stored 
## in a makeCacheMatrix() 'object'.

makeCacheMatrix <- function(x = matrix) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
                get <- function() x
                setinv <- function(inv) m <<- inv
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## Check for a cached inverse calculation. 
        m <- x$getinv()
        if(!is.null(m)) {
                ## If cached calculation exists, fetch it.
                message("getting cached data")
                return(m)                 
        }
        ## If no cached calculation then run solve().
        data <- x$get() 
        m <- solve(data)
        x$setinv(m)
        m ## Return inverse calculation.
}
