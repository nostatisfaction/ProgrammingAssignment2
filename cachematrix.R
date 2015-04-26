## Put comments here that give an overall description of what your
## functions do
#
# The functions provided below are used to cache and calculate the inverse of
# an invertible matrix. The first function makeCacheMatrix is used to cache a 
# new matrix and returns a list containing the functions necessary to access
# and manipulate the cached version of that matrix (stored as x) and its
# inverse (stored as inv). Calling makeCacheMatrix does not cache the inverse,
# however. This is instead accomplished using cacheSolve, which accepts the
# function list returned by makeCacheMatrix and uses it to ( a ) calculate,
# cache and return the inverse of the currently cached matrix if it has not 
# already been calculated; OR, ( b ) return the cached inverse (inv) of the 
# cached matrix if it is already cached (as tested using is.null()).
#
# These functions were created using the provided examples (makeVector and 
# cachemean) as templates and are therefore coded quite similarly.


## Write a short comment describing this function
#
# makeCacheMatrix
#   Input: The sole argument (x) should be an invertible matrix
#
#   Output: The function will return a list containing four functions: set()
#   may be used to set the cached matrix (x) to a new, provided value while also
#   clearing any stored inverse; get() may be used to return the currently
#   cached matrix; setinverse() calculates and caches the inverse of a provided
#   matrix; getinverse() returns the cached inverse.
#
#

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) inv <<- solve(mat)
    getinverse <- function() inv
    return(list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse))
}


## Write a short comment describing this function
#
# cacheSolve
#   Input: The sole argument (x) should be a function list created by calling 
#   makeCacheMatrix().
#
#   Output: The function will return the inverse of the matrix cached by the
#   function makeCacheMatrix(). If an inverse is currently cached, it will
#   return that. Otherwise it will calculate and cache the inverse of the
#   currently cached matrix and return that.
#
#

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
