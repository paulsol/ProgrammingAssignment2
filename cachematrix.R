## Create a Matrix that can store/cache its inverse to avoid 
##     repeating the solve() function on the matrix.
## The makeCacheMatrix will copy the supplied matrix and
##     create/return a list consisting of 4 functions to 
##     1. store the matrix, 2. get the matrix, 
##     3. solve/invert it and store the resulting matrix
##     4. return the stored inverse if requested again.
## cacheSolve() can be run on the object created by makeCacheMatrix
##   to return the inverse of the original matrix.
##   If the stored/cached inverse is not empty (the inverse was 
##   initialized to NA as well as reset to all NAs if matrix was 
##   changed using $set() function in list), then the stored inverse
##   is returned.
##   Else it will use solve() of the base package to create the inverse
##   which is stored as well as returned.  


## makeCacheMatrix will store a copy of a matrix passed to it, together with
##   a function to set this stored matrix to new values, a function to retrieve
##   the stored matrix, a function to store it's inverse, and a function to 
##   retrieve the saved inverse.  The methods are returned in a list.

makeCacheMatrix <- function(x = matrix()) {
    invrs_x <- matrix(NA,nrow(x),ncol(x))   ## initiate as NA w/ same rows/cols as x
    set <- function(y) {
        x <<- y
        invrs_x <<- matrix(NA,nrow(y),ncol(y))
    }
    get <- function() x
    setinvrs <- function(invrs) invrs_x <<- invrs
    getinvrs <- function() invrs_x
    list(set = set, get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)
}


## cacheSolve uses the solve() base function to find the inverse of the 
##   stored matrix plus store a copy of the inverse.  To avoid unneeded
##   repetition of solve(), first check to see if the stored inverse is 
##   available (using x$getinvrs fnctn) and return that if only NA.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvrs()
    if(!is.na(inv[1,1])) {
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    inv <-solve(data)
    x$setinvrs(inv)
    return(inv)
}
