#    The two functions below read in a matrix and determine the inverse wich is
#                    then cached for later use.
# ------------------------------------------------------------------------------
# The function makeCacheMatrix creates a matrix object that is a cache to store
#  the inverse of a matrix.
#
makeCacheMatrix <- function(x = matrix()) {
    # Create a vector in which to cache the inverted matrix
    #    and set the values to NULL
    invx <- NULL
    # Obtain the values in the matrix to be inverted (x) and store in set.matr
    set.matr <- function(y) {
    # note that `<<-` assigns a value to an object in an environment 
    # different from the current environment.
        x <<- y
        invx <<- NULL
    }
    # get the matrix to be inverted
    get.matr <- function() 
        x
    # Obtain the inverse of the matrix (x) using the function "solve" 
    #  and store in set.invx 
    set.invx <- function(solve) 
        invx <<- solve
    get.invx <- function() 
        invx
    list(set.matr=set.matr, get.matr=get.matr, set.invx=set.invx, get.invx=get.invx)
}
#
# ------------------------------------------------------------------------------
#
# The function cacheSolve calculates the inverse of the matrix object
# returned by makeCacheMatrix on the first run, and thereafter retrives the
# inverse from the cache
#
cacheSolve <- function(x, ...) {
    # x is the original matrix to be inverted from  makeCacheMatrix()
    # x$getinv() returns the inverse of the original matrix x and 
    #  stores it in the vector invx
    invx <- x$get.invx()
    # If this is the second run and the inverse has already been calculated
    # we skip the computation and get it from the cache.........
    if (!is.null(invx)){
        message("getting cached data")
        return(invx)
    }
    # .... but if this is the first run we determine the inverse using
    #        the function solve() and store it in the vector invx
    mat.data <- x$get.matr()
    invx <- solve(mat.data, ...)
    # We set the value of the inverse in the cache set.invx
    x$set.invx(invx)
    return(invx)
}
#
# ------------------------------------------------------------------------------
#
