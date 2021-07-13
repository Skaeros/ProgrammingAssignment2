### INTRODUCTION
##  Script creates 2 functions - one to create an object for the purposes of
#   storing the inverse solution to an input matrix 'x'; the other to use the
#   functions/values stored in the created object to either extract a cached
#   inverse for input matrix 'x', or calculated and store it for later use


### FUNCTION #1
##  makeCacheMatrix fucntion takes a matrix input (assumed invertible) and
#   creates an object containing 4 functions:
#   (1) 'set' resets the cache when the function is run (for a new vector)
#   (2) 'get' can be used to extract the stored input matrix 'x'
#   (3) 'setinv' can be used to cache a calculated inverse matrix
#   (4) 'getinv' can be used to extract a previously cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

### FUNCTION #2
##  cacheSolve function populates and/or retrieves the inverse of an input
#   matrix from the object created by makeCacheMatrix function.
#   Taking the output of makeCacheMatrix, it firstly tries to retrieve an
#   inverse... if one is found in the cache it will retrieve it, if one isn't
#   found it will calculate using solve() and use setinv() to cache the result.
#   Finally - returns the inverse of the initial input matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
