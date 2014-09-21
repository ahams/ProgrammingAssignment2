makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  y<-NULL
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setmat = setmat,getmat = getmat,setinv = setinv, getinv = getinv)
}
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
  }
  mat <- x$getmat()
  m <- solve(mat, ...)
  x$setinv(m)
  return (m)
}
