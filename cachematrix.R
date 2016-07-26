
## R- Programming Assignment 2. cache matrix inversion with lexical scoping.


##makeCacheMatrix creates a matrix object that can 
## cache its inverse.
## if the stored matrix is reset the inverse object is set to null.

makeCacheMatrix  <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setMatrix <- function(matInverse) mInverse <<- matInverse
  getMatrix<- function() mInverse
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## cacheSolve function calculates the inverse of a matrix using R function.
##if the inverse matrix is already present in the cache, it retrieves from the cache.
## Assumption is that the input matrix is inversible. (per assignment).

cacheSolve<- function(x, ...) {
  mInverse <- x$getMatrix()
  if(!is.null(mInverse)) {
    message("getting cached inverse matrix")
    return(mInverse)
  }
  message("Inverse,not in cache: running inversion on the matrix")
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setMatrix(mInverse)
  mInverse
  ## Return a matrix that is the inverse of 'x'
}

#Sample output
# > source("cachemean.R")
# > mat<-matrix(1:4,nrow=2,ncol=2)
# > mat
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mvec <- makeCacheMatrix()
# > mvec$set(mat)
# > mvec$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mvec$getMatrix()
# NULL
# > cacheSolve(mvec)
# Invesrse,not in cache: running inversion on the matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mvec$getMatrix()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mvec)
# getting cached inverse matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 

