## Here we are caching a matrix and binding the values of x (the matrix) and s (solve) in functions stored in a list

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## This function gets the stored value of s and determines whether it is cached or not
## If it is (not null), it returns s. If not, is calls solve and stores the value.

cacheSolve <- function(x, ...) {

  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
  
}