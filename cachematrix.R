## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. Our goal is to write 2 functions -
# one of them 'makeCacheMatrix' and 'cacheSolve' that cache the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) k <<- solve
  getsolve <- function() k
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function. this function computes the inverse of the special 'matrix' returned by 'makeCacheMatrix' above. If the inverse hasn't changed - then if should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  k <- x$getsolve()
  if(!is.null(k)) {
    message("getting inversed matrix")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setsolve(k)
  k
  }


#Testing:

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
# Everything is correct)