## cachematrix  
## This package contains two functions which cache the inverse of a matrix serving to avoid computing it repeatedly.
##
### Usage example:
### source("cachematrix.R")
### testmatrix=rbind(c(1,-1/4, 1/8,7/8), c(1,1, 1,1), c(1/8,-1/4,1,-3), c(1/25,5, -3/4, 5))
### specialmatrix<-makeCacheMatrix(testmatrix)
### invertedtestmatrix<-cacheSolve(specialmatrix)
### invertedtestmatrix2<-cacheSolve(specialmatrix)
### "getting cached data"
###
### To confirm the inversion worked:
### solve(invertedtestmatrix)
### solve(invertedtestmatrix2)

##################
## makeCacheMatrix 
## This function creates a special matrix object that can cache its inverse.
## Parameters:
##   x is the matrix whose inverse is to be cached
## Return:
##   list of methods available to access and manipulate cache
makeCacheMatrix <- function(x = matrix()) {
    ## data cache
    ###  set up a location in which to cache our inverted matrix 
    m <- NULL
    
    ## define methods
    ### initialize x to the incoming matrix and m to null
    set <- function(y) {
        #### this operator assigns y from another environment ie calling function
        #### to x in the current environment ie this function
        x <<- y
        #### initialize m to null to serve as a flag for empty or filled cache
        m <<- NULL
    }
    ###  get the value of the incoming matrix
    get <- function() x
    
    ###  set the value of the inverse
    #### 
    setinverse <- function(solve) m <<- solve
    ###  get the value of the inverse
    getinverse <- function() m
    
    ## cache methods as a list of key-value pairs 
    ### each list element's label is the same as the method name
    ### this allows the calling function to access the methods via the labels
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#############
## cacheSolve
## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix is UNCHANGED) the matrix is retrieved from the cache.
## Parameters:
##   x is a square invertible matrix
## Return: 
##   m is a matrix which is the inverse of x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
