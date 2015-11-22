## Coursera: Programming Assignment 2
## Author: ruaridhw

## This R file contains two functions as per the requirements
## of the Coursera course R Programming: Assignemnt 2

## The function makeCacheMatrix is designed to create a special matrix
## object that can cache its inverse. It will check whether or not the
## matrix inverse has already been computed and cached before executing
## the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  # Clear the variable inv
  inv <- NULL
  
  # Create the function set which stores the matrix data
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # Create the function get which retrieves the matrix data
  get <- function() x
  
  # Create the functions setinverse and getinverse which
  # cache and retrieves the matrix inverse respectively
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set,
       get = get,
       setinverse = setinverse
       getinverse = getinverse)
  
}

## The function cacheSolve first checks that an inverse has not
## already been cached before computing the inverse of the matrix x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Execute the function getinverse
  inv <- x$getinverse()
  
  # Check that the inverse was successfully cached and retrieved
  if(!is.null(inv)){
    message("Cached data successfully retrieved")
    # If the inverse was found then exit the functino
    return(inv)
  }
  
  # Retrieve the matrix data
  matrixdata <- x$get()
  
  # Compute the matrix inverse
  inv <- solve(matrixdata, ...)
  
  # Cache the inverse
  x$setinverse(inv)
  
  # Return the matrix inverse
  return(inv)
}