# These functions are used to cache a matrix to the memory and to compute the inverse of this matrix.
# The makeCacheMatrix accepts a matrix as an input and releases a list functions related to the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The seconnd function computes the inverse of the matrix that was sent to the cache in the previous function (makeCacheMatrix).
#If the cache is not NULL, it returns it, otherwise it loads the matrix from cache and uses the solve() function to return
# the inverse matrix


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    print("get from cache")
    return(inverse)
  }
  matrix <- x$get()
  
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
