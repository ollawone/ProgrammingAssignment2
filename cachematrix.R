## This function creates a special "matrix" object that can cache its inverse.
## Therefore caching potentially time-consuming computations.

makeCacheMatrix <- function(x = matrix()) {
  # This fucntion is set up in stages
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse
  # 4. get the inverse
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(inverse) m <<- inverse
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
  
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## If x is not already calculated then
  ## do it
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(mat.data, ...)
  x$setm(m)
  m
  
}
