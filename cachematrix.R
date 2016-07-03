## Created by John Bell on 3/7/2016 in answer to the programming assingnment
## Threre are three functions in this file: makeCacheMatrix, cacheSolve, ranmat
## :: makeCacheMatrix creates a list of functions for Caching a matrix with its 
##    inverse
## :: cacheSolve returns the inverse of the matrix stored in the cached matrix
##    if the inverse is already cached it returns the cached matrix if not
##    it calculates it and caches the result
## :: ranmat generates a matrix of random variables for testing the other 
##    two funtions

## This function creates a list of four functions for storing a matrix with its
## inverse. 
##  1. The first function in the list (set) sets the value of the cached matrix
##     and resets the inverse matrix to NULL
##  2. The second function in the list (get) returns the value of the cached matrix
##  3. The third funciton in the list (setinv) sets the value of the inverse matrix
##  4. The forth funtion in the list (getinv) gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv,  getinv = getinv)
}


## This function uses the matrix cached in the special list funtion created by
## the first funtions. It returns the inverse of the cached matrix.
## It first checks whether the inverse has been cached, if so it returns the 
## cached inverse. If not it calculates the inverses and caches the result using
## the setinv function from the first function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}

## Creates a square matrix of random variables of size x to test 
## the other two functions

ranmat <- function(x, ...) {
    matrix(rnorm(x^2, ...), nrow = x)
}
