## makeCacheMatrix needs to run before cacheSolve. The purpose of makeCacheMatrix
## is to receive a square matrix x and place it in a list object, which can then be accessed
## by the functions get(), set(), getInverse(), setInverse().

## get() returns the square matrix
## set() creates the square matrix when called from a makeCacheMatrix object (that's why it uses the <<- operator)
## getInverse() looks for cached inverse of the square matrix
## setInverse() is the function that will run from cacheSolve to create 
##    the square matrix (that's why it uses the <<- operator)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  getInverse <- function() inverse
  
  # allow this function to set this value outside its scope by using <<- operator
  setInverse <- function(solve) inverse <<- solve
  
  # return a list object
  list( set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve returns the value of the matrix if it's already been solved. Otherwise, the function uses the solve
## method proposed in the homework instructions to do the calculation

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # return the solved matrix if the calculation has already been performed
  
  if( !(is.null(inverse))) {
    message("getting the cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  
  # solve the inverse matrix
  inverse <- solve(x$get())
  
  #pass it back to the makeCacheMatrix object
  x$setInverse( inverse )
  
  inverse
}
