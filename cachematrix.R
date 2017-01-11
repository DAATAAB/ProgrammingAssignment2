## The following functions cache and return the inverse of a
## given matrix

## The "makeCacheMatrix" function prepares the inverse of the matrix

makeCatcheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The "cacheSolve" function returns the inverse of the given matrix
## If the inverse is stored in the cach, beside the inverse returns a message

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
