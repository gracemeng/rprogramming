## Return a list containing function to set/get matrix and set/get the inverse

## The list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  x$setinv(inv)
  inv
  
}
