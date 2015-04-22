## The two functions below cache the inverse of a matrix to reduce the computational time


## This function creates a special "matrix" object that can cache its inverse 
## But in fact it returns a list of functions to set/get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function() {x}
  setinv <- function(inv) {m <<- inv}
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x is the list created by makeCacheMatrix
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
