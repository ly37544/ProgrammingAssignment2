## Put comments here that give an overall description of what your
## functions do
## This program caches the inverse of a given matrix if its invers 
## matrix has been figured out previously or computes the inverse 
## matrix if not.

## Write a short comment describing this function

## This function built below is aimed for creation of a object that 
## can cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invdata) inv <<- invdata
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## This function either computes the inverse of the given
## matrix if needed or just return the already cached one if 
## not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  invdata <- solve(data, ...)
  x$setinv(invdata)
  invdata
}
