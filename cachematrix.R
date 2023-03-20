## A pair of functions that cache the inverse of a matrix


##creates a matrix that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  ##get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##calculated the inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ##checks if the inverse is already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}