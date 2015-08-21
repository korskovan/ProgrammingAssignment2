# -*- coding: utf-8 -*-

## makeCacheMatrix returns a list of 4 functions: get/set matrix, get/set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates inverse matrix if it's not already calclulated

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {  ## inverse is already calculated
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
 
  inverse
}
