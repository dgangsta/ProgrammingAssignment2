## These functions provide a special object that can be used to optimize inverse calculations.
## Matrices returned by makeCacheMatrix are capable for storing their original values and their inverse.
## The inverse is calculate by the cacheSolve method which is optimized by caching and utilizing previously
## calculated values.  

## Creates a special matrix object that is capable of caching its inverse.
## Args: 
##    x: Any invertible matrix
## 
## Returns:
##   A special cacheable matrix providing getters and setters for its original and inverse values
## Assumption: All matrices provided are invertible.

makeCacheMatrix <- function(x = matrix()) {

  
  # Define s in terms of the makeCacheMatrix environment
  s <- NULL
  # setter
  set <- function(y) {
    x <<- y  # x is a free variable here, lexical scoping will be used
    s <<- NULL # This will clear out any assigned solve function
  }
  # getter
  get <- function() x
  # set cached solve function result
  setsolve <- function(solve) s <<- solve
  # get cached solve function result
  getsolve <- function() s
  # Return the matrix functions which will share a scope
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Computes the inverse of the special matrix object created by makeCacheMatrix.  If the inverse
## has already been calculated, the cached value is returned.  If not, the inverse is calculated, 
## cached, and returned.
## Args: 
##    x: Any matrix returned by the makeCacheMatrix function 
## Returns:
##   The inverse of the matrix stored in the provided object 


cacheSolve <- function(x, ...) {  
  # The inverse is calulated using the solve function with a single parameter.  
  # When a single parameter is used, the solve matrix will provide its identity matrix 
  # as the second parameter.  This is effectively the inverse of the first parameter 
  # when the first is invertible.
  
  # See if the vector has solve function set and returns it
  s <- x$getsolve()  # get the mean and then call it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # Calculate & cache the results of the solve function
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

