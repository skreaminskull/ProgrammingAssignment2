## R Programming
# Programming Assignment #2
# Write a pair of functions that cache the inverse of a matrix.

## Functions descriptions below:
  
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix(). If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve() should return the inverse from the cache to avoid re-computing it.
 

## create a special matrix object, that will cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # intialize to NULL
  m <- NULL  
  # intialize/set matrix values by making call to matrix function
  set <- function(y) {
    x <<- y
    # set to NULL everytime set is called in order to not retrieve cached data if matrix contents changed
    m <<- NULL
  }
  # return matrix values
  get <- function() x
  # function to call solve and generate inverse matrix
  setInverse <- function(solve) m <<- solve
  # function to pass values of inverted matrix
  getInverse <- function() m
  # object with 4 functions in "special" matrix object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the cached inverse, if the inverse has been calculated previously and the matrix has not changed. Otherwise, call solve() to compute the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # test if inverse already exists, if so return the cached data. If the values in the matrix has changed via calling the set() function in makeCacheMatrix, the value of m is set to NULL and the inverse is computed again by passing the matrix values to the solve function and then setting those values in the cached variable m.
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  
  # if inverse doesn't exist, compute and set it 
  # get matrix values
  data <- x$get()
  # call solve to compute inverse
  m <- solve(data, ...)  
  # set values for inverted matrix from previous step
  x$setInverse(m)
  m
}