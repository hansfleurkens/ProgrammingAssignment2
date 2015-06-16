# Functions to calculate the inverse of the matrix using a cache function, that
# caches the result of the matrix inversion. If the input matrix is unchanged
# and a previous result is available, the result is returned from the cache
# to save a intensive computation of the inverse.


# Function: makeCacheMatrix
# 
# Argument(s):
#   x   A square matrix
#
# Returns a list with member functions that can be used for cached matrix
# inversion, in order these elements are
#       - set the value of the matrix
#       - get the value of the matrix
#       - set the inverse matrix
#       - get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # Functions to set and get the value of the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      # Functions to set and get the inverse matrix
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      # Return functions in special object
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
# Function: cacheSolve
# 
# Argument(s):
#   x  List that contains 4 elements
#       - to set the value of the matrix
#       - to get the value of the matrix
#       - to set the inverse matrix
#       - to get the inverse matrix
#
# Returns a matrix that is the inverse of the matrix stored in the input list.

cacheSolve <- function(x, ...) {
      # Check for cached data
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # get matrix from input list and calculate inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
