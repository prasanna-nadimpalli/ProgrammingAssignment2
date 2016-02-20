## The functions below use the concepts of lexical scoping in caching matrix
## Inverses

## returns a list of functions that get/set a matrix and get/set inverse of a matrix

makeCacheMatrix <- function(matrixforinverse = matrix()) {
       i <- NULL
       set <- function(y) {
              matrixforinverse <<- y
              i <<- NULL
       }
       get <- function() matrixforinverse
       setinverse <- function(solve) i <<- solve
       getinverse <- function() i
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## first checks if inverse of matrix exists in cache. if exists returns from cache else computes inverse of mqatrix

cacheSolve <- function(x, ...) {
       inversematrix <- x$getinverse()
       if(!is.null(inversematrix)) {
              message("getting cached data")
              return(inversematrix)
       }
       data <- x$get()
       inversematrix <- solve(data, ...)
       x$setinverse(inversematrix)
       inversematrix
}
