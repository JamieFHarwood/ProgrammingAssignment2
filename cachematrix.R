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

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Take the result of a call to makeCacheMatrix as input and return the 
## inverse of the input's matrix field

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
