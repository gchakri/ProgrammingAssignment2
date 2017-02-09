## These two functions will allow us to to serach if a inverse 
## of a matrix is available already, and calculates only if
## it isn't cached

## The "MakeCacheMatrix" function sets and gets a matrix to
## to inverse. It also caches if there is a solved inverse matrix 
## and allows it to be accessed through getinverse and 
## setinverse operations.

makeCacheMatrix <- function(x = matrix()) {
	invX <- NULL
	set <- function(y) {
		x <<- y
		invX <<- NULL
	}
	get <- function() x
	setinverse <- function(solvedInv) invX <<- solvedInv
	getinverse <- function() invX
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This "cacheSolve" function checks if an inverse matrix is 
## stored in the previous function and returns if it finds
## It only computes an inverse if a cached solution isn't there
## and will store it once it computes

cacheSolve <- function(x, ...) {
	invX <- x$getinverse()
	if(!is.null(invX)) {
		message("getting cached inverse matrix")
		return(invX)
	}
	mat <- x$get()
	invX <- solve(mat, ...)
	x$setinverse(invX)
	invX
      ## Return a matrix that is the inverse of 'x'
}
