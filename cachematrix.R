## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## COMMENT for the PEER EVALUATION!
  ## this function returns an inverse-cache-able matrix object
  inv_cache <- NULL
  set <- function(y){
      x <<- y
      inv_cache <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_cache <<- inv
  getinv <- function() inv_cache
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  #### COMMENT for the PEER EVALUATION!
  ## Return a matrix that is the inverse of 'x'
  inv_cache <- x$getinv()
  if(!is.null(inv_cache)){
      message('getting cache data')
      return(inv_cache)
  }
  data <- x$get()
  inv_cache <- solve(data, ...)
  x$setinv(inv_cache)
  inv_cache
}
