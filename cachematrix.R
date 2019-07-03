# This file defines CacheMatrix, which is a matrix that can cache its inverse.
# As matrix inverse operation is costly, this will be useful if you need to
# calculate the inverse of the same matrix multiple times.
# Eg use:
# x <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
# xInverse <- cacheSolve(x)
# x$get() %*% xInverse == Identity matrix


# Returns an object of class CacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
  # Class variables. Martrix and its inverse.
  matrix <- x
  inverseMatrix <- NULL

  # Getters and setter.
  get <- function() matrix
  set <- function(y) {
    matrix <<- y
    inverseMatrix <<- NULL
  }

  # Gets the cached inverse of the matrix.
  # Computes the inverse if it is not already calculated or if recompute is TRUE.
  # recompute can be set to TRUE if you are chaning the paramaters passed to
  # solve.
  getCachedInverse <- function(recompute = FALSE, ...) {
    if (recompute || is.null(inverseMatrix)) {
      message("Computing matrix inverse")
      inverseMatrix <<- solve(matrix, ...)
    } else {
      message("Returning cached inverse")
    }
    inverseMatrix
  }

  cacheMatrix <- list(get = get, set = set, getCachedInverse = getCachedInverse)
  attr(cacheMatrix, "class") <- "CacheMatrix"
  cacheMatrix
}


# Returns the inverse of x which must be an object of class CacheMatrix.
cacheSolve <- function(x, ...) {
  if (class(x) != "CacheMatrix") {
    stop("This function only works for objects of class CacheMatrix")
  }
  x$getCachedInverse(...)
}

# A simple test for CacheMatrix class. This just checks for the correctness of
# the inverse.
testCacheMatrix <- function() {
  matrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
  identityMatrix <- diag(2)
  for (i in 1:10) {
    matrixInverse <- cacheSolve(matrix)
    if (!all(matrix$get() %*% matrixInverse == identityMatrix)) {
      stop("Matrix inverse wrong")
    }
  }
  message("Test passed")
  matrix
}
