## Functions to Cache inverse of a matrix

## makeCacheMatrix
## This function takes a matrix object as input and 
## creates a list of these sub-functions:
##  1. set the matrix
##  2. get the matrix
##  3. set inverse of the matrix
##  4. get inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) x_inv <<- inverse
  get_inverse <- function() x_inv
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## cacheSolve
## This function takes the matrix (created by makeCacheMatrix)
## as input and checks if the inverse of the matrix was already
## calculated. If the inverse matrix is available in cache, it 
## returns the cached contents, instead of recalculating the
## inverse all over again
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$get_inverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$set_inverse(x_inv)
  x_inv
}

x = matrix(rnorm(4), 2, 2)
my_matrix <- makeCacheMatrix(x)
my_matrix$get()
my_matrix$get_inverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
cacheSolve(my_matrix)
