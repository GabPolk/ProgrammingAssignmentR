## 'makeCacheMatrix' and 'cacheSolve', work together to cache the inverse of 
## a matrix. The goal is to avoid recalculating the inverse of the matrix if it 
## has already been calculated previously, saving computational time.

## 'makeCacheMatrix' creates an "object" that contains a matrix and can store 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve' calculates the inverse of the matrix created by 'makeCacheMatrix'. 
## If the inverse has already been calculated, it is returned from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
