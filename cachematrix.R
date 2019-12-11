## The overall purpose of these functions is to reduce time consuming computations for
## finding the inverse of matrices by being able to cache the inverse for later use.


## makeCacheMatrix's purpose is to create a matrix object that can cache its inverse,
## but it will actually a be list that contains a function to set the value of the
## matrix, get the value of the matrix, set the value of the inverse, and get the value
## of the inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv = NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve's purpose is to actually solve for the inverse of the special matrix
## that is created from the function makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
