 
## The functions below are used to create a special "matrix"
## object and cache its inverse. 


## The first function, makeCacheMatrix creates a special “matrix” object,
## which is really a list containing a function to:
        
 ##       1. set the value of the matrix

 ##       2. get the value of the matrix

 ##       3. set the value of the inverse

 ##       4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m  <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


 
## This function computes the inverse of the special "matrix" 
## returned by the above function, makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix has not
## changed), then cachesolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m )
        m 
}
 
