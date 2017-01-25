## "makeCacheMatrix" and "cacheSolve" will create a specialized matrix object
## that can cache it's inverse in "makeCacheMatrix" 
## and then compute the inverse in "cacheSove".

## In the same vein as the "makeVector" example, we 1. set a value for the matrix,
## 2. get the value of the matrix 3. set the value of the inverse 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL;
set <- function(y){
  x <<- y;
  m<<- NULL;
}
get <- function() x
setInverse <- function(inverse.gaussian) m <<- inverse.gaussian
getInverse <- function() m
list(set = set, 
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see
## if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse
##in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse.gaussian(data,...)
  x$setInverse(m)
  return(m)
}
