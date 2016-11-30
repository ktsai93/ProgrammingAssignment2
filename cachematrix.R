## makeCacheMatrix() creates a list containing functions that
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

## cacheSolve(x, ...) does the following:
## 1a) if inverse already exists in the list returned from makeCacheMatrix(),
##    then the inverse is returned and the function completes.
## 1b) otherwise, the inverse of the matrix from the list gets calculated
## 2b) the inverse gets stored into the list
## 3b) the inverse is returned and the function completes


## makeCacheMatrix creates the list that holds setter and getter functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
