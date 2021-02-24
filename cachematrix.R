## The first function creates 4 other functions to set the matrix, get the 
## matrix, set the inverse, and get the inverse respectively.

## The second function checks whether the inverse of matrix x has already been
## calculated. If it is, then the function will return the value. If it is not,
## then the function will calculate the inverse, store it with the setinv
## function, and finally return the value

## This is the first function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This is the second function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}