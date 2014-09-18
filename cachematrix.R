## Coursera - R Programming
## Programming Assignment 2 (peer assessment): Lexical Scoping
## 
## Version : 1.1 -- Minor changes into the comments
##
## This script contains two functions that are used to create a special object 
## that stores a matrix and caches its inverse
##


## makeCacheMatrix function creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Variable initialization  
    m_cache <- NULL
    
    ## Set the value of the matrix
    setMatrix <- function(y) {
        x <<- y
        m_cache <<- NULL
    }
  
    ## Get the value of the matrix
    getMatrix <- function() x
    
    ## Set the value of the inverse matrix
    setInverseMatrix <- function(my_matrix) m_cache <<- my_matrix
    
    ## Get the value of the inverse matrix
    getInverseMatrix <- function() m_cache
    
    ## Create the list
    list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## cacheSolve function calculate the inverse of matrix

cacheSolve <- function(x, ...) {
    ## Variable initialization 
    m_cache <- x$getInverseMatrix()
    
    ## if the inverse of X is already in cache, then it simply returns the cache
    if(!is.null(m_cache)) {
        message("getting cached matrix")
        return(m_cache)
    }
  
    ## if the inverse is not in cache, then it calculates it, and puts it into the cache
    MatrixData <- x$getMatrix()
    m_cache <- solve(MatrixData)
    x$setInverseMatrix(m_cache)
    
    m_cache
}

## End of script
