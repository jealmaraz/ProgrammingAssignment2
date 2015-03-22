## These functions allowed to cache the inverse of a matrix to save time on
## potentially time consuming computations. It can only be used with square 
## invertible matrices.


## The first function creates a list with functions to 1. set the value of the 
## matrix, 2. get the value of the matrix, 3. get the inverse of the matrix, and
## 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- solve(x)
        getInverse <- function() i
        list(set = set, get = get,
              setInverse = setInverse, getInverse = getInverse)
}


## If the inverse of the matrix has been previously calculated, this function
## returns a message together with the inverse of the matrix. If it hasn't,
## then it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getInverse()
          if(!is.null(i)) {
                  message("getting cached data")
                  return(i)
          }
          data <- x$get()
          i <- solve(data,...)
          x$setInverse(i)
          i        
}
