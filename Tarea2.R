###With this two functiones, is possible to get the inverse of a matrix, and get use
##for solve a ecuations system


##This function, provide us a type of matrix that we will use as a input to print the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##This function will print us the inverse of the matrix


cacheSolve <- function(x, ...) {
 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}