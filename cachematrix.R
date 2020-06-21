## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and creates a 
## special matrix object caching the value and your inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix<-function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix<-function() x
  setInverseMatrix <- function(invMatrix) inverseMatrix <<- invMatrix
  getInverseMatrix <- function() inverseMatrix
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setInverseMatrix = setInverseMatrix,
    getInverseMatrix = getInverseMatrix
  )
  
}


## cacheSolve takes the return of makeCacheMatrix as input 
## check if the inverse o matrix is cached
## If it exists in cache, return the cached value
## else computes the inverse and set its value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$getMatrix()
  inverseMatrix <- solve(data)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
