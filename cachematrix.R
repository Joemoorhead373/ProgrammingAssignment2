## First function creates a  "special" matrix that contains a list that contains a function
## to set the matrix, get the matrix, set the inverse, get the inverse
## Second function calculates the inverse of the above matrix

## First function contains a list that contains a function
## to set the matrix, get the matrix, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Inverse function obtained from matlib package
  
  get <- function() x
  setinverse <- function(Inverse) m <<- Inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Second function calculates inverse from the above matrix, first checks
## if inverse already calculated, if so it gets inverse from cache and skips
##computation. Otherwise it calculates inverse of matrix
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


