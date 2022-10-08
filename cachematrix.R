## Pair of functions that cache the inverse of a matrix.

## Below function creates a special "matrix" (which is really a list containing a function) object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { ##sets the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x ##gets the value of the matrix
  setinverse <- function(inverse) i <<- inverse ##sets the value of the inverse
  getinverse <- function() i ##gets the value of the inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## ## Below function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
