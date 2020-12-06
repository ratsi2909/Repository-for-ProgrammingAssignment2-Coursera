## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set <- function(y) {  # set or reset value of matrix
  x <<- y  # operator can modify variables at parent level
  inv <<- NULL
}
get <- function()x  # get the value of matrix 
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # get the return value from makeCacheMatrix
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
