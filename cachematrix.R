## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object which allows caching of its inverse
## parameter/argument x is an invertible matrix, defaulting to an empty matrix if x is not defined
## A list is returned with operations for matrix setting/getting, and inverse matrix setting/getting
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix supplied to makeCacheMatrix
## if the inverse has already been calculated, the cached version is used and returned without 
## having to calculated/add extra overhead. If it has not been calculated prior, it is calculated, and then cached for
## future retrievals
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
