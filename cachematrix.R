rm(list = ls())
library(simpleCache)


# creates a matrix, invert the matrix, load matrix from cache
makeCacheMatrix <- function(m = matrix()) {
	iv <- NULL
      
	setMatrix <- function(y) {
		m <<- y
		iv <<- NULL
 	} 
  
	getMatrix <-function() m
  	setinverse <- function(invM) iv <<- invM
      
      getinverse <- function() simpleCache("inverse")

  	list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse,
           getinverse = getinverse)
}
# The following function inverts a matrix created with the above function.
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the inverted matrix from the cache and skips the computation.
# Otherwise, it creates and sets in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	invM <- x$getinverse()
      
	if(!is.null(invM)){
      	message("getting cached inverse matrix")
 	}
 
 	data <- x$getMatrix()
	invM <- solve(data)
      print(x$setinverse(invM))

      cacheDir <- tempdir()
	setCacheDir(cacheDir)
      simpleCache("inverse", {x$setinverse(invM)}, recreate = TRUE)
      print( listCaches())
     
	print(invM)
 
}


M <- matrix(c(1:4), 2, 2)
func <- makeCacheMatrix(M)
cacheSolve(func)


