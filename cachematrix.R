## The following functions implement a cached matrix
## inversion suite.  Using the following functions,
## a user can calculate the inverse of an inputted
## matrix and retrieve the previously calculated
## value without caching it him or herself.

## This function creates a special matrix object that
## exports four different manipulation functions:
##  set(x), which takes a matrix and sets the internally
##      represented matrix to this new value (and clears
##      any cached inverse state).
##  get(), which returns the stored matrix.
##  setinverse(x), which sets the cached inverse of the matrix
##      to "x".
##  getinverse(), which returns the cached inverse of the given
##      matrix.  Otherwise, it returns NULL.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(a_matrix) {
        x <<- a_matrix
        cached_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(a_matrix) cached_inverse <<- a_matrix
    getinverse <- function() cached_inverse
	# Return a list of four functions described above.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes two groups of arguments:
##  1. A "cached-inverse matrix" (from the makeCacheMatrix function).
##  2. Any additional arguments to the "solve" function.
##
## The goal of this function is to calculate the inverse of
## the matrix parameter in the special matrix "x".  If the
## inverse has been calculated once for an unchanged matrix,
## this function will provide a cached version from the previous
## calculation.  Otherwise, if the matrix has since changed (or
## if it has yet to be calculated), it will calculate the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
	## If the inverse has been calculated previously,
	## return that cached data instead of recalculating.
    if (!is.null(inverse)) {
        message("Returning cached data.")
        return(inverse)
    }
    a_matrix <- x$get()
    inverse <- solve(a_matrix, ...)
    x$setinverse(inverse)
    inverse
}
