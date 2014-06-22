
## The functions perform the following:

## makeCacheMatrix: creates a special "matrix" object that is able to cache its inverse.

## cacheSolve:      computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                  If the inverse has already been calculated (and the matrix has not changed), then
##                  cacheSolve should retrieve the inverse value from the cache.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value for the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  
  ## get the value for the inverse of the matrix
  getinverse <- function() m
  
  ## create the list to wrap the 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## by calculating the inverse of the special "matrix" created with the above function. 
## It checks first to see if the inverse has previously been calculated. 
## If it has, it gets the inverse value from the cache and does not recalculate. 
## Otherwise, it calculates the inverse of the data, sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## Get matrix m from the cache
  m <- x$getinverse()
  
  ## If m is NULL this indicates the inverse has been previously calculated
  ## so instead of recalculating, return the cached value
  if(!is.null(m)) {
    message("getting cached data")
    ## return the cached inverse
    return(m)
  }
  
  data <- x$get()
  ## Calculate the inverse
  m <- solve(data, ...)
  ## Set the inverse value in the cache
  x$setinverse(m)
  ## Return inverse
  m
}