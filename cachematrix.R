makeCacheMatrix <- function(x = matrix()) {  ## function to cache inverse matrix data
  m <- NULL
  set <- function(y) {                       ##setting the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                        ## getting the value of the matrix
  setinverse <- function(solve) m <<- solve  ## setting the value of the inverse matrix using solve function
  getinverse <- function() m                 ## getting the value of the inverse matrix  
  list(set = set, get = get,               
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheInverseMatrix <- function(x, ...) {    ## funtion to call data cached before.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()                           ## if the data is not cached, then evaluate usong solve.
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
