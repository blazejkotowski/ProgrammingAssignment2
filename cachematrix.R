# This program consists of two functions that together serve a purpose
# of caching the inverse of a matrix, instead of calculating it
# every time it is requested.

# Create a list of methods to provide functionality of caching
# the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  x <- x
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  
  list(get=get, set=set, setInverse=setInverse, getInverse=getInverse)
}


# This function checks if the inverse is already cached in the
# given list, and if it is already cached, it returns the value
# without calculation, otherwise, it calculates the inverse,
# caches it in the list and returns it.
cacheSolve <- function(x, ...) {
  if(!is.null(x$getInverse())) return(inv);
  
  inv <<- solve(x$get(), ...)
  x$setInverse(inv)
  
  inv
}
