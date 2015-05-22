## Cashing the inverse of a matrix
## 

## a function that creates a special matrix
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse matrix
  setinv <- function(solve) m <<- solve
  
  # get the inverse matrix
  getinv <- function() m
  
  # list() function that stores the 4 functions in the function makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # get the inverse of the matrix
  m <- x$getinv()
  
  # if the inverse matrix is already computed, it will skip computation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # else it gets the matrix, computes its inverse and return it 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
