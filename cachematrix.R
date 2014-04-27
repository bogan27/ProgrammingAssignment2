## There are two functions in this R file that handle caching the inverse of a matrix

## This first function creates a list with 4 components:
## "set" updates the values of the cached matrix
## "get" returns the values of the cached matrix
## "setinverse" updates the values of the cached inverse
## "getinverse" returns the values of the cached inverse

## The later 2, setinverse and getinverse, are referenced in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) m <<- solve
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function takes in a matrix, and checks to see if it's inverse has been computed
## If it has, it prints "getting cached data" and returns the inverse
## Otherwise, it computes the inverse, returns it, and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
