## The code below consist of two related functions that can be used to
## save computation time in case you've to calculate the inverse of the
## same matrix several times.
    ## The first one can be used to calculate and cache the inverse
    ## of a matrix. (NB: the matrix is assumed to be invertable)
    ## The second one can be used to return the inversed matrix from 
    ## the cache.

## Function name: `makeCacheMatrix`
## Explanation:  This function creates a special "matrix object"
## that can calculate and cache the inverse of the input matrix x.
## The special "matrix object" can be called to with 4 different affixes:
    ## '$set(?)' -> loads the new matrix '?' into the function
    ## '$get() -> presents the loaded matrix 
    ## '$setinverse(?) -> calculates the inverse of matrix '?'
    ## '$getinverse()' -> presents the inverse of the loaded matrix (if already calculated)

makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
         set <- function(y) {
                 x <<- y
                 i <<- NULL
         }
         get <- function() x
         setinverse <- function(solve) i <<- solve(x)
         getinverse <- function() i
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }


## Write a short comment describing this function
## Function name: `cacheSolve` 
## This function computes the inverse of the "matrix object"* returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## and the matrix has not changed), then `cacheSolve` retrieves the inverse
## from the cache.
## (* 'y' in the code below refers to the stored matrix object of the 
## function above.)

cacheInverse <- function(y, ...) {
        i <- y$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- y$get()
        i <- solve(data, ...)
        y$setinverse(i)
        i
}
