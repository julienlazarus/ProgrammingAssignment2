## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
  x <<- y
  cm <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cm <<- inverse
  getInverse <- function() cm
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by 
## the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cm <- x$getInverse()
  if (!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  mat <- x$get()
  cm <- solve(mat, ...)
  x$setInverse(cm)
  cm
}

