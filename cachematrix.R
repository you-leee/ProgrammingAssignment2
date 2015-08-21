## The makeCacheMatrix function creates 3 functions for caching the input matrix and its inverse.
## After you called the makeCacheMatrix function with a invertible matrix, you can retrieve its value.
## The cacheSolve function calculates the inverse of the given matrix.
## When you call the cacheSolve function again, it returns the cached value without calculating the inverse again.
## Example:
##	> x <- makeCacheMatrix(matrix(c(1,3,4,5,6,12,3,8,11),3,3))
##	> cacheSolve(x)
##	     	  [,1] [,2] [,3]
##		 [1,]  -30  -19   22
##		 [2,]   -1   -1    1
##		 [3,]   12    8   -9
##	> cacheSolve(x)
##	getting cached inverse matrix
##       	  [,1] [,2] [,3]
##		 [1,]  -30  -19   22
##		 [2,]   -1   -1    1
##		 [3,]   12    8   -9


## Creates a list of functions for caching an invertible matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Args:
    ##  'x' is an invertible matrix
    
    ## Returns:
    ##  A list of functions:
    ##      *'get': get the value of the matrix 'x'
    ##      *'setinverse': set the value of the inverse matrix 'invX'
    ##      *'getinverse': get the value of the inverse matrix 'invX'
    
    invX <- NULL
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        invX <<- inverse
    }
    getinverse <- function() {
        invX
    }
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the matrix, if it hasn't been calculated. 
##  If it has been calculated already, it returns the cached value of the inverse.

cacheSolve <- function(funcs, ...) {
    ## Args:
    ##  'funcs' a list of functions:
    ##      *'get': get the value of the matrix 'x'
    ##      *'setinverse': set the value of the inverse matrix 'invX'
    ##      *'getinverse': get the value of the inverse matrix 'invX'
    
    ## Returns:
    ##  The inverse of the matrix 'x'
    
    invX <- funcs$getinverse()
    if(!is.null(invX)) {
        message("getting cached inverse matrix")
        return(invX)
    }
    x <- funcs$get()
    invX <- solve(x, ...)
    funcs$setinverse(invX)
    invX
}
