## Script containing two functions:
##
## 1. makeCacheMatrix - Creates list object containing 
##    set/get methods for a matrix and setInverse/getInverser
##    methiods for the inverser of the matrix.
## 2. cacheSolve - Retrieves and returns inverse matrix from list object 
##    created by makeCacheMatrix. If inverse is not set, cacheSolve
##    creates the inverse and sets list objects inverse property.

## makeCacheMatrix - creates list object containing 
##    set/get methods for a matrix and setInverse/getInverser
##    methiods for the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {

    vInverseOfX <- NULL
    
    setX <- function(pMatrix){
        x <<- pMatrix
    }
    
    getX <- function() x
    
    setInverse <- function(pInverse) {
        vInverseOfX <<- pInverse
    }
    
    getInverse <- function() vInverseOfX
    
    list(setX = setX, getX = getX,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve - Retrieves and returns inverse matrix from list object 
##    created by makeCacheMatrix. If inverse is not set, cacheSolve
##    creates the inverse and sets the list object's inverse property.
cacheSolve <- function(x, ...) {
    
    vInverse <- x$getInverse()
    if(!is.null(vInverse)) {
        message("Returning cached inverse")
        return(vInverse)
    }
    message("Calculating inverse")
    vMatrix <- x$getX()
    vInverse <- solve(vMatrix, ...)
    x$setInverse(vInverse)
    vInverse
    
}
