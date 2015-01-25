

## The following cache function deals with potentially time-consuming computations
## It was designed as an assignment for the Coursera R Programming Course (Week 03)


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function () inv
  
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix above
## If the inverse had been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
    if(!is.null(inv)) {
          message("getting chached data")
          return(inv)
    }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  message("Done.")
  return(inv)
}

## My test matrix

tm <- matrix(c(1,3,5,7,9,11,13,17,23), 3, 3)
tm

m = makeCacheMatrix(tm)
m$get()

cacheSolve(m)
m$getinv()


