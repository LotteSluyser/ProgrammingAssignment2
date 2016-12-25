## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## initialize the value of the matrix inverse to NULL
        m <- NULL
        set <- function(y) {
                ##assign the input argument to the x object in the parent environment
                x <<- y
                ##assign the value of NULL to the m object in the parent environment.
                m <<- NULL
        }
        ## R retrieves the value of x from the parent environment of makeCacheMatrix().
        get <- function() x
        #calculates the inverse of the matrix with the solve function
        setinverse <- function(solve) m <<- solve
        # gets the inverse   
        getinverse <- function() m
        ## passes the value of the function makeCacheMatrix    
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # if m is not 0 it gets the cached data and mentions that
        if (! is.null(m)) {
                message ("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}