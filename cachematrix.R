#
# Matrix inversion is usually a costly computation.

# There may be some benefit to caching the inverse of a
# matrix rather than compute it repeatedly. The functions
# below implement a simple way to speed up matrix inversion
# by caching results of previous matrix inversion in memory,
# and retrieving the cached inversion results.
#
## Example
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(x)

## No cache in the first run
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333


## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## 

# makeCacheMatrix creates a list containing a function to 
# cache the inverse of a matrix in a cache environment
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse)
    { inv <<- inverse }
  
  getinverse <- function() 
    { inv }
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# cacheSolve(matrix)
#
# The cacheSolve function returns the inverse of the matrix.
# cacheSolve first attempts to get the inverse of the matrix
# from cache (using the getinverse accessor). If the inverse
# exist in memory, then it is returned. If not, cacheSolve 
# performs the inversion, caches the result and returns an
# inverted matrix.
#
# This function assumes that the matrix is always invertible.x
#

cacheSolve <- function(x, ...) {
    
  inv <- x$getinverse()
       
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
  
  
