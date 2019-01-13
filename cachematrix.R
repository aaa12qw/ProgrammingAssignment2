## Put comments here that give an overall description of what your
## functions do

## makeVector creates a special "vector"
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse_temp <- NULL
    set <- function(y) {
        x <<- y
        inverse_temp <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse_temp <<- inv
    get_inverse <- function() inverse_temp
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_temp <- x$get_inverse()
    if(!is.null(inverse_temp)) {
        message("getting cached data")
        return(inverse_temp)
    }
    data <- x$get()
    inverse_temp <- solve(data)
    x$set_inverse(inverse_temp)
    inverse_temp
}
