## First, I can create a matrix, then I allow the object to cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {inv <<- inverse}
  getInv <- function() {inv}
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse. It either gets it from the cache. If it's NULL, it creates the inverse first, then returns it.


cacheSolve <- function(x, ...){
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    inv
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
