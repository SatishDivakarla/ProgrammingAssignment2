## This program primarily calculates the inverse of the matrix. It also maintains a cache for
#  inverse of matrices. 

## This function is used to construct a matrix and it returns three functions : 
#  Set : Initialize a matrix
#  Get : Returns the initialized matrix,  
#        If not initialized, returns an empty 1x1 matrix with "NA" as value.
#  setinverse : This method is called from cacheSolve function
#  getinverse : Returns the inverse of matrix initialized by set.
#               If matrix is not initialized, it returns NULL

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function looks if the inverse for the matrix is always constructed.
#   If Yes: It returns the inverse from the cache
#   Else: It constructs inverse using solve function, sets it to cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
