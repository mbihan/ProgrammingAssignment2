## This program will take vectors in the form of matrix, cache the inverse of matrix. There is an assumption that
## the matrix is always invertible
## There are two functions for this purpose "makeCacheMatrix" and "cacheSolve"

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        print (x)
        i <- NULL
        
        # Set the value of matrix
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        
        # Get the value of matrix
        get <- function()x
        
        # Set the inverse of matrix
        setinverse <- function(inverse) i <<- inverse
        
        # Get the value of inverse matrix
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        # Check if the inverse of matrix has been calculated and cached
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        # If the inverse is not cached, then claculates matrix that is inverse 
        data <- x$get()
        i <- solve(data, ...)
        
        # Cache the calculated inverse matrix 
        x$setinverse(i)
        i
}
