makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(mat) {
    x <<- mat
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
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
