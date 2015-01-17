#Creates a special "matrix" object that can cache its inverse
# Args:
#   x: a square invertible matrix object
#
# Returns:
#   A special "matrix" based on a list object that provides 
#   a get/set methods to the original matrix object and a
#   get/set methods to the inversed (and cached) matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Checks whether or not whe have a cached inversed matrix. 
# Returns the cached object if found, otherwise computes the inverse matrix using R "solve" method
#
# Args:
#   x: a square invertible matrix object
#
# Returns:
#   An inverse matrix of x matrix (cached or computed)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  
  data <- x$get()
  message("computing inverse matrix")
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

