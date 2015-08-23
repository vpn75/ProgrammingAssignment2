## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix argument and computes the inverse using the solve function.
## The matrix inverse is cached for future use. The function returns a list containing functions
## to set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function() m <<- solve(x)
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve() function searches for a cached matrix inversion stored from the above
## function and returns that if found. Otherwise it computes the matrix inverse using the 
## solve() function and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
}
