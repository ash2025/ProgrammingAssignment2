## Matrix inversion is usually a costly computation and there may be some
## benefit  to caching the inverse of a matrix rather than compute it 
##repeatedly. These two functions are used to cache the inverse 
##of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of inverse of the matrix
# 4 get the value of inverse of the matrix

makeCacheMatrix <- function(A = matrix()) {
  AI <- NULL
  set <- function(B) {
    A <<- B
    AI <<- NULL
  }
  get <- function() A
  setinverse <- function(inverse) AI <<- inverse
  getinverse <- function() AI
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function returns the inverse of the matrix. It first checks
## if the inverse has already been computed. If so, it gets the result and 
## skips computation. If not, it computes the inverse, sets the value in 
## the cache via setinverse function.

# This function assumes the matrix is always invertible(nonsingular square)

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
  AI <- A$getinverse()
  if(!is.null(AI)) {
    message("getting cached data")
    return(AI)
  }
  data <- A$get()
  AI <- solve(data, ...)
  A$setinverse(AI)
  AI
}

##A<-matrix(c(8,2,3,4,7,6,3,8,9),3,3)
##dit(A)
##x<-makeCacheMatrix(A)
## A %*% cacheSolve(x)
