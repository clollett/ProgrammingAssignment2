## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates the structure for our cache, set and get which
## are the mutator and accessor functions of the original matrix
## setsolve and getsolve are functions to mutate or access the inverted matrix
## which in the case of not being calculated yet, it should return NULL
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
  
}


## Write a short comment describing this function
## it takes advantages from makeCacheMatrix
## when it is called, it checks if the inverse has already calculated 
## in X$getsolve in the case of still not being calculated, it calculates it and 
## store it using X$setlove, if it has already being calculated, it just return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
