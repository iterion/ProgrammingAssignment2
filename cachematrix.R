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
       setmean = setmean,
       getmean = getmean)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
