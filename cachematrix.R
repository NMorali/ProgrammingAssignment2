## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing the original matrix and additional functions
## cacheSolve calcultes the inverse of a matrix using the output of the previous  function
## if the result is already stocked i the cach it is read from there otherwise it is computed again

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the inverse ot NULL
  i <- NULL
  
  ## Set function to reset the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## creation of the output: the list of the matrix and additional functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

## If the inverse has already been cached, the function gets it from the cache, otherwise it is recalculated

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## checks if the inverse has already been calculated, if so reads it from cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## since the inverse has never been calculated it is calculated
  data <- x$get()
  i <- solve(data, ...)
  
  ## the metadata of the makecachematrix object is updated with the value of the inverse so next time the value will be read from cache and not computed again
  x$setinverse(i)
  i
}
