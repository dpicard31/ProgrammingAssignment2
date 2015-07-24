## makeCacheMatrix will create a matrix object that can cache its inverse
## cacheSolve will compute the inverse of makeCacheMatrix, or retrieve from 
## cache if it already has been computed

## makeCacheMatrix will create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Returns a matrix that is the inverse of 'x' , checks cache first

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
