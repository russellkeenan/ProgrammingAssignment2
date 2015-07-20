## The two functions below work together to provide the inverse matrix, caching the calculation
## The solution is closely based upon the example in the assignment

## makeCacheMatrix creates a special "matrix", which is really a list containing functions to
##
## 1. set : set the value of the matrix
## 2. get : get the value of the matrix
## 3. setinv : set the value of the inverse matrix
## 4. getinv : get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  # 'inv' is the cached inverse matrix
  inv <- NULL

  # internal definitions of functions using 'inv' via closure
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv

  # returns list of methods so that they are available to callers
  list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the mean of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the inverse in the cache via the
## setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()

  if(!is.null(m)) {
    message("using cached inverse")
  } else {
    message("calculating inverse")
    origmatrix <- x$get()
    m <- solve(origmatrix, ...) #solve returns inverse of matrix
    x$setinv(m)
  }

  m 
}
