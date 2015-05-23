## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
