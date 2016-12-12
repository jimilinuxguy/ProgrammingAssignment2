## Put comments here that give an overall description of what your
## functions do
## Calculating the inverse of a matrix can be very expensive to compute, so it is beneficial for us to store it cached, 
## so that future references to it can be pulled without being recomputed. 
##
## makeCacheMatrix creates a list  with methods for setting and getting the value of the matrix as well as setting and 
## getting the value of the inverse of the matrix. 
## makeCacheMatrix does not perform any calculations
##
## cacheSolve returns the inverse of a matrix. If the inverse of  matrix has already been computed, the cached version 
## is returned, if not, then the inverse of the matrix is computed and the cached value is stored and returned.  

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object which allows caching of its inverse
## parameter/argument x is an invertible matrix, defaulting to an empty matrix if x is not defined
## A list is returned with operations for matrix setting/getting, and inverse matrix setting/getting
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL #initialized to null so that we know if cachesolve has been previously run
  set <- function(y = matrix()) { ## Sets the value of the matrix
    x <<- y ## <-- assignment operator is used in functions and checks throughout parent environments
            ## for an existing variable being assigned and if found, then the value is redefined, else 
            ## it stores it in the global environment.
    m <<- NULL # Set to null whenever set is called, since we know that the cached version should not  
                 # be returned until this is computed again
  }
  
  get <- function() { # Gets the value of the matrix
    return(x)
  }
  setinverse <- function(inverse) { # Sets the value of the inverse matrix
    m <<- inverse
  }
  getinverse <- function() { # Gets the value of the inverse matrix
    return(m)
  }
  
  list(
      set = set, 
      get = get, 
      setinverse = setinverse, 
      getinverse = getinverse
    )
}


## cacheSolve returns the inverse of the matrix supplied to makeCacheMatrix
## if the inverse has already been calculated, the cached version is used and returned without 
## having to calculated/add extra overhead. If it has not been calculated prior, it is calculated, and then cached for
## future retrievals
### ... elipsis means function can take any number of arguments
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
