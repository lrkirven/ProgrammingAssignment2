## Put comments here that give an overall description of what your
## functions do

## Creates a matrix and returns a list containing functions
## to update the matrix' for inversr caching

makeCacheMatrix <- function(x = matrix()) {
    myInv <- NULL
    set <- function(y) {
      x <<- y
      myInv <<- NULL
    }
    get <- function() x
    setinv <- function(i) myInv <<- i
    getinv <- function() myInv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks if I have a cached value of the matrix.  If I do,
## return the cached value. If not, calculate the inverse
## of the incoming matrix via the solve function and cache 
## the inverse value for the next call.

cacheSolve <- function(x) {
  invX <- x$getinv()
  if(!is.null(invX)) {
    message("Getting cached inverse")
    return(invX)
  }
  matrixData <- x$get()
  invX <- solve(matrixData)
  x$setinv(invX)
  invX
}
