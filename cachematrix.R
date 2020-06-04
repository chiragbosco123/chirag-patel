## This two functions will help in caching the inverse of the matrix. Very long vector may take
## too long to compute the mean. Thus below two functions helps in
## storing the matrix and caches its inverse, thus saving the time 
## This programming is done using the Lexical scoping  


## Below mentioned function is creating a matrix that helps in caching its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Below function computes the inverse of the special matrix which was created by makeCacheMatrix.
## if the inverse has been computed it will retrive the value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
  }

