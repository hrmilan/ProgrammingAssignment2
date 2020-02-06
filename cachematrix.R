## The two functions defined below will first store a matrix in a
## cache format and calculate its inverse. The concept is used to
## reduce time-consuming operations, such as calculating a matrix'
## inverse, by storing it in cache and retrieving its value.

## The makeCacheMatrix function creates a matrix that can cache 
## its inverse. It sets the value of the matrix, gets its value,
## sets its inverse and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function calculates the inverse matrix stored
## at the makeCacheMatrix function. If the inverse matrix was 
## already calculated, it simply retrieves its cached value. If
## not, it calculates it using the above defined setsolve function.


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
