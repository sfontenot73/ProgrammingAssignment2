## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
