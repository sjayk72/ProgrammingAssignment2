## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # m is inverted matrix
        m = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setm = function(inverse) m <<- inverse 
        getm = function() m
        # list all the output
        list(set=set, get=get, 
             setinv=setm, getinv=getm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m = x$getinv()
        
        # if there is inverted matrix on cache, get it from cache
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        # if not, calculate the inverse 
        matrix <- x$get()
        m = solve(matrix, ...)
        
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
