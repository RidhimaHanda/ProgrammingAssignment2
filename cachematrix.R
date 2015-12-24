## These functions check if the inverse of the matrix is available
## If the inverse is available, it returns the value. If not, it calculates the inverse of the matrix

## This function contains four functions: set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<-y
    m <<-NULL
  }
  get <- function(y) x
  setinverse <- function(inverse) {
    m <<- inverse
  }
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function checks if the inverse of a matrix is already available, if so it pulls the cached value, if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
