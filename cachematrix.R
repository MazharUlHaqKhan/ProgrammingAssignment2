##  This function creates a special "matrix" object
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted<-x$getinverse()
  if(!is.null(inverted)){
    return(inverted)
  }
  originalmatrix<-x$get()
  inverse <-solve(originalmatrix)
  x$setinverse(inverse)
  inverse
}

