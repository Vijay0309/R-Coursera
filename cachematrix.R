## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function makeCacheMatrix creates the cache matrix from the given matrix
makeCacheMatrix <- function(x = matrix()) {
	
	cachedMatrix <- NULL
	
	#setting the matrix
	setMatrix <- function(y){
		x <<- y
		cachedMatrix <<- NULL
	}
	
	#getting the matrix
	getMatrix <- function()x
	
	#setting the cached matrix
	setCachedMatrix <- function(invMatrix){
		cachedMatrix <<- invMatrix
	}
	
	#getting the cached matrix
	getCachedMatrix <- function()cachedMatrix

	#return the list of methods to access
	list(setMatrix=setMatrix,getMatrix=getMatrix,setCachedMatrix=setCachedMatrix,getCachedMatrix=getCachedMatrix) 	
}


## Write a short comment describing this function
# The function cacheSolve calculates the inverse of a given matrix
# This method does the following.
# 1. Finds if the inverse of given matrix is available in cache, if avalable returns the value from cache
# 2. If not found in cache then calculates the inverse of matrix then stores in cache & return the value.
cacheSolve <- function(x, ...) {

	result <- x$getCachedMatrix()
	
	if(!is.null(result)){
		message("result found in cache...")
		return(result) 
	}
	
	givenMatrix <- x$getMatrix()
	result <- solve(givenMatrix)
	x$setCache(result)
	return(result)
    ## Return a matrix that is the inverse of 'x'
}
