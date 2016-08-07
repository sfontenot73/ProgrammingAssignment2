## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. This file contains
## pair of functions that cache the inverse of a matrix.

##  This function creates an object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix<<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inverse_matrix <<- inverse
  getInv <- function() inverse_matrix
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from cache, and print a message
        inverse_matrix <- x$getInv()
        if (!is.null(inverse_matrix)){
              message("getting cached data")
              return(inverse_matrix)
        } ##If no inverse matrix exists, get the matrix and find the inverse
        matrix <- x$get()
        inverse_matrix <- solve(matrix, ...)
        x$setInv(inverse_matrix)
        inverse_matrix
}
