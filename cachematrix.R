## Builds a special Matrix that can cache the inverse of itself when
## using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set the matrix and clear the cached value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  # cache the inverse in i 
  setInverse <- function(inverse) i <<- inverse
  # retieve cached value
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Using the makeCacheMatrix solve the matrix and cache it 
## so that multiple cacheSolve calls will not perform the expensive
## solve operation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check to see if it's cached
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached value")
    return(i)
  }
  # not cached, so solve and store the value in the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
