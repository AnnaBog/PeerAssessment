## The following function called 'makeCacheMatrix' creates a list
## containing a function which sets the value of a matrix,
## gets the value of a matrix, sets the value of an inverse of the matrix,
## and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The 'cacheSolve' function calculates the inverse of the matrix created by the function above,
## first checking if the inverse of the matrix has already been calculated.
## If so, it gets the inverse of the matrix, and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets its value
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
