## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(mat) {
    x <<- mat
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve computes the inverse of the matrix returned by 
# makeCacheMatrix function above. If the inverse has already
# been calculated and the matrix is same, then the cachesolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse<- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
