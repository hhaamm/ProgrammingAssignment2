# This function creates a matrix wrapper with some useful methods
# like set, get, setinverse and getinverse
# So this is like a "class" with two "properties": matrix and inverse
# with their respective getters and setters.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix"
# If the inverse has already been calculated (and the matrix has not changed),
# then this returns the inverse from the cache
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}

# Example output of testing this code:
# > c <- makeCacheMatrix(matrix(c(4,7,2,6), 2, 2))
# > c2 <- cacheSolve(c)
# > c2 <- cacheSolve(c)
# getting cached data
# > c$get() %*% c2
# [,1]         [,2]
# [1,] 1.000000e+00 1.110223e-16
# [2,] 1.110223e-16 1.000000e+00
# > round(c$get() %*% c2)
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1