## Put comments here that give an overall description of what your
## functions do

##This function implements the basic functionality of getting and setting, i.e. overall computing of the
##Cache  Matrix. matrix is a list containing funcitons to get  and set the original matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  set <- function(y) {
    x <<- y
    temp <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) temp <<- inverse
  getinverse <- function() temp
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the matric created by the makeCacheMatrix function and if the inverse has been calculated we just retrieve it.
## Otherwise, we solve.
cacheSolve <- function(x, ...) {
  temp <- x$getinverse()
  if (!is.null(temp)) {
    message("getting cached invverse")
    return(temp)
  }
  data <- x$get()
  temp <- solve(data, ...)
  x$setinverse(temp)
  temp
}
