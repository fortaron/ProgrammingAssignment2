## Caching the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) v <<- solve
  getInverse <- function() v
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by the previous function "makeCacheMatrix"
## Retrieve cached data if is already calculated

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if(!is.null(v)) {
    message("Getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setInverse(v)
}


