## This program shows what lexical scoping does through creating 
## and caches a special "matrix" and solves for its inverse.

## This function creates a special list-type object "matrix" that has 
## 4 attributes: set, get, set_inverse, and get_inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m    
}
#################################

# Construct a matrix from a vector
newMatrix <- matrix(rnorm(16), 4)
newMatrix

# Use makeCacheMatrix and cacheSolve
anotherMatrix <- makeCacheMatrix(newMatrix)
attributes(anotherMatrix)
class(anotherMatrix)
cacheSolve(anotherMatrix)
cacheSolve(anotherMatrix)


# Clear the environment
remove( list=ls() )
