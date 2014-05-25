## These 2 functions can be used together to efficiently
## determine the inverse of a given matrix.
##
## makeCacheMatrix takes a square, invertable matrix
## as its input and provides methods for setting and getting
## both the value of the base matrix and the inverse of this base
## matrix
##
## cacheSolve takes the output of makeCacheMatrix and checks to see
## if the inverse of the base matrix has already been calculated.
## If it has it returns the known inverse value.  If it has not the 
## inverse is calculated, stored in the makeCacheMatrix object and then
## returned as the result.


## Create an object that stores a given matrix and a mechanism
## for storing the inverse of this matrix

makeCacheMatrix <- function(baseMatrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    baseMatrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() baseMatrix
  setinverse <- function(matrix) inverse <<- matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Take the result of a call to makeCacheMatrix as input and return the 
## inverse of the input's matrix field

cacheSolve <- function(mcmObj, ...) {
  tmpInverse <- mcmObj$getinverse()
  if(!is.null(tmpInverse)) {
    return(tmpInverse)
  }
  baseMatrix <- mcmObj$get()
  tmpInverse <- solve(baseMatrix, ...)
  mcmObj$setinverse(tmpInverse)
  tmpInverse
}
