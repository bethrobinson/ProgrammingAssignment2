## This set of functions can be used when the inverse of a matrix
## may need to be calculated repeatedly with data that may or may not change. 
## It will cache the solution to be called later when needed,
## saving on overall calculation time.

## Create a set of four small functions stored in a list.
## Each function can be called separately.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinvert <- function(solve) m <<- solve
   getinvert <- function() m
   list(set = set, get = get,
        setinvert <- setinvert,
        getinvert <- getinvert)
}


## This function solves for the inverse of a set of data in a matrix
## If the inverse was calculated before, then it will pull the stored value.

cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
       message("getting cached data")
       return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m        
}
