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
    
    ### initialize x to the incoming UNINVERTED matrix y and m to null
    set <- function(y) {
        #### this operator assigns UNINVERTED matrix y from another environment ie calling function
        #### to x in the current environment ie this function
        x <<- y
        #### initialize m to null to serve as a flag for empty ie UNINVERTED or filled cache
        m <<- NULL
    }
    
    ###  get the value of the incoming matrix
    get <- function() x
    
    ###  set the value of the inverse
    #### 
    setinverse <- function(solve) m <<- solve
    ###  get the value of the inverse previously calculated and stored in m
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
    ## x is the matrix objects which (possibly) has a cached inverted matrix available, accessed via get inverse method
    m <- x$getinverse()
    
    ## if we DID find a cached inverted matrix, get it stored in the m cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise we go ahead and call the get method to get the incoming matrix
    data <- x$get()
    ## then we run solve to invert the incoming matrix
    m <- solve(data, ...)
    ## finally we store the inverted matrix in m
    x$setinverse(m)
    ## and return m
    m
}
