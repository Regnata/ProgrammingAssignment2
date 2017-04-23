## Programming Assignment 2 
## Caching the Inverse of a Matrix in order to avoid repeated costly inverse computations  


## Function creates a special matrix object in order to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function computes the inverse of the special matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}



# Test results
mx <- matrix(c(3, -7, 5, 2), 2,2)
mxcache <- makeCacheMatrix(mx)
mxinv <- cacheSolve(mxcache)
mxinv

