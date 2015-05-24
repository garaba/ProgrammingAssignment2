## This function  is able to cache potentially time-consuming inversion of a matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## Get matrix
  setinv <- function(solve) m <<- solve(x)  ## Invert matrix
  getinv<- function() m  ## Get computed inverted matrix, imatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the matrix returned by the above function

cacheSolve <- function(x, ...) {
  m <- x$getinv()  ##Gets the imatrix from cache
    if(!is.null(m)) {   ##Checks imatrix existence in cache
      message("getting cached data")
      return(m)   ##Returns the imatrix from cache
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)  ##Stores imatrix in cache
        m    ## Returns imatrix, the inverse of 'x'
}
