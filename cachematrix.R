## Assignment Caching invmatrixerse of a Matrix:
## makeCacheMatrix: Function that creates a special "matrix" object, which cache its invmatrixerse.
## 1.set the value of vector
## 2.get the value of vector
## 3.set the value of mean
## 4.get the value of mean

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinvmatrixerse <- function(invmatrixerse) invmatrix <<- invmatrixerse
  getinvmatrixerse <- function() invmatrix
  list(set = set,
       get = get,
       setinvmatrixerse = setinvmatrixerse,
       getinvmatrixerse = getinvmatrixerse)
}

## cacheSolve: 
## This function computes the invmatrixerse of the special "matrix" returned by above makeCacheMatrix . 
## If invmatrixerse has already been calculated (and the matrix has not changed), cacheSolve should retrieve the invmatrixerse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the invmatrixerse of 'x'
  invmatrix <- x$getinvmatrixerse()
  if (!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  mat <- x$get()
  invmatrix <- solve(mat, ...)
  x$setinvmatrixerse(invmatrix)
  invmatrix
}

# Output of this function as below
# > matrix <- makeCacheMatrix(matrix(2:5, 2, 2))
# > matrix$get()
#        [,1] [,2]
#  [1,]    2    4
#  [2,]    3    5
# > cacheSolve(matrix)
#       [,1] [,2]
#  [1,] -2.5    2
#  [2,]  1.5   -1
