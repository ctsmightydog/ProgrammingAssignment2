## Matrix inversion can take a lot of computation time and there may be some benefit to
## caching the inverse of a matrix if it is necessary to compute it many times.
## The following two functions are used to cache the inverse of a matrix.


## makeCacheMatrix creates a list containing 4 functions that:
## 1. set the value for a matrix
## 2. get the value for a matrix
## 3. set the value for the inverse of the matrix
## 4. get the value for the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse_m<<- inverse
  get_inverse <- function() inverse_m
  list(set=set, get=get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## cacheSolve returns the inverse of a matrix. It first checks if the inverse 
## has already been computed. If so, it gets the result and skips further computations.
## If not, it computes the inverse and sets the value in the cache via the 
## set_inverse function.


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## this assumes the matrix is always a square and invertible.
  inverse_m <- x$get_inverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  data<-x$get()
  inverse_m <- solve(data, ...)
  x$set_inverse(inverse_m)
  inverse_m
}
