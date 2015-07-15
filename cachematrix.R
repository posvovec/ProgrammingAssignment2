## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of functions. The first one is used when we'd like to change the matrix. 
#The second function get the initial matrix. The third one, gets the matrix after inversion
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
   x <<- y
   m <<- NULL
  }
  get <- function() x
  setinverse <- function(invert) m <<- invert
  getinverse <- function() m
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function do all the work. If it's first time we use the function it calls the setinverse() from makeCacheMatrix, 
##than compute the inverse function and save it into m. The value of m is in the environment of cacheSolve

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}
