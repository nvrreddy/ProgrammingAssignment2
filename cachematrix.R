##A pair of functions to create a matrix and store its inverse in cache which can be retrived as required.

## The below function creates a special matrix object and stores its inverse in cache object.

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  getMatrix <- function() x
  
  cacheInverse <- function(solve) cache <<- solve
  
  getInverse <- function() cache
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
  
}


##The following functionretrieve the inverse of the special matrix from the cache if it has alredy been computed, else it will compute the inverse
cacheSolve <- function(x, ...) {
  ##This function returns matrix that is inverse of x
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  inverse
  
}
