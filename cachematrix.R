## The following is an assignment: Caching the Inverse of a Matrix. 

# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * setCahced      set the cahced value (inverse of the matrix)
# * getCahced      get the cahced value (inverse of the matrix)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     setMatrix <- function(y) {
          x <<- y
          m <<- NULL
     }
     getMatrix <- function() {
          x
     }
     setCahced <- function(solve) {
          m <<- solve
     }
     getCahced <- function() {
          m
     }
     list(setMatrix = setMatrix, getMatrix = getMatrix, setCahced = setCahced, getCahced = getCahced)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
     m <- y$getCahced()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- y$getMatrix()
     m <- solve(data)
     y$setCahced(m)
     
     m
}
