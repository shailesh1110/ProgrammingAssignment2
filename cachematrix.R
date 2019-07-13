
## MakeCacheMatrix function creates a special matrix object that can cache its inverse.

MakeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(y) {
    X <<- y
    inv <<- NULL
  }
  get <- function() X
  setInverse <- function() inv <<- solve(x) #calculate inverse here
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special matrix returned by MakeCacheMatrix above and returns the value if already calculated

CacheSolve <- function(x, ...) {
    ## Return a matrix that is the X inverse.
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("retriving cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
