## The following are two functions that are used to cache the inverse of a matrix.


## Creates a special matrix object that can cache its inverse

## The first function, makeCacheMatrix, creates a special "matrix", 
## which is essentially a list containing a function to -
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse
  i <- NULL
  
  ## Function to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Function to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Function to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse 
    i
  }
  
  ## Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function, cacheSolve, is used to Compute the inverse of the special 
## matrix returned by "makeCacheMatrix"above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if it is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}