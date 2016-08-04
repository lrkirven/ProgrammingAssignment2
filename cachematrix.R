## These functions cache previous calculated inverse of a matrix to avoid 
## possible time-consuming computations.

## Creates a matrix and returns a list containing functions
## to manage and update the matrix' for inversr caching

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


## Checks if the matrix list has a cached value of the matrix. If it 
## does, return the cached value. If not, calculate the inverse
## of the matrix via the solve function and cache (or store) the 
## inverse value for the next call.

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
