## The functions developed here help to preserve computational efforts while 
## working with matrix inversion which is a computationally intensive operation
## The functions represent a framework where matrix inversion results are cached 
## so that subsequent inversion request for same matrix can get the result from 
## cache, saving computational efforts
## USAGE:-
## step 1: cacheMatrix <- makeCacheMatrix(original matrix)
## step 2: inverseMatrix <- cacheSolve(cacheMatrix)
## NOTE on USAGE: clubbing the 2 steps will prevent leveraging of cache feature
## therefore avoid: inverseMatrix <- cacheSolve(makeCacheMatrix(matrix)) as it is 
## not computationally efficient

## makeCacheMatrix function builds an object which acts as a repository of matrix
## and its inverse having the following capabilities:-
## matrix data storage - x - passed as formal argument
## inverse matrix data storage - xInv
## get / set functions for the matrix - getMatrix(), setMatrix(matrix)
## get / set functions for the inverse matrix - getInverse(), setInverse(matrix)
makeCacheMatrix <- function(x = matrix()) {
        ## if x not square matrix give caution message
        if (nrow(x) != ncol(x)) {
                message("Caution=>Not a square matrix. Inverse not defined.")
        }
        
        ## get / set functions for matrix & inverse
        xInv <- NULL
        setMatrix <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) xInv <<- inverse
        getInverse <- function() xInv
        
        ## list of functions
        list(
                getMatrix       = getMatrix,
                getInverse      = getInverse,
                setMatrix       = setMatrix,
                setInverse      = setInverse
        )

}

## cacheSolve function is to be used by end user to obtain the inverse of a matrix
## input is the original matrix as a makeCacheMatrix object
## output is the inverse of the original matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInverse()
        if(!is.null(xInv)) {
                message("getting cached inverse")
                return(xInv)
        }
        matrixData <- x$getMatrix()
        xInv <- solve(matrixData)
        x$setInverse(xInv)
        xInv
        
}
