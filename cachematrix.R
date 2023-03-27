## These functions create a special "matrix" object than can cast its inverse, and 
## then compute the inverse of the special "matrix". If the inverse has already been calculated,
## the cachesolve will retrieve the inverse from the cache

## Creates the special "matrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(invertedmatrix) m <<- invertedmatrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)  
}


## Checks to see if the matrix has already been inverted. If yes, the result is retrieved from the cache.
## If no, the function calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
