## These two functions allow a user to cache the inverse computation on
## a matrix. This is beneficial in that the inverse will not need to be
## recomputed for each iteration of a loop.

## In this function we create 4 methods that allow the user to get
## and set a cached matrix, as well get and set the value for the inverse
## of the matrix.
## makeCacheMatrix returns a list of these 4 functions so that a user 
## can appropriately update the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks if the inverse has been calculated and cached.
## If it was available it returns it immediately. If not, it calculates the
## inverse, caches it for future requests, and then returns the inverse.

cacheSolve <- function(x, ...) {
  #try to get inverse and return immediately if available
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }

  #no cache was present, so calculate the inverse and cache it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
