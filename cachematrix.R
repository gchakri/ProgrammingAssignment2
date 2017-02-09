## These two functions will allow us to to serach if a inverse 
## of a matrix is available already, and calculates only if
## it isn't cached

## The "MakeCacheMatrix" function creates a matrix with get and
## set functions so that a special matrix can be created/accessed
## in/from different envirnment. It also caches if there is 
## an inverse matrix solved for the matrix and stores it and allows
## access through getinverse and setinverse operations.
## Once we assign a new matrix it sets a NULL value!

makeCacheMatrix <- function(x = matrix()) {
	iX <- NULL
	set <- function(y) {
		x <<- y
		iX <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) iX <<- solve
	getinverse <- function() iX
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This "cacheSolve" function checks if an inverse matrix is 
## stored in the previous function and returns if it finds
## It only computes an inverse if a cached solution isn't there
## and will store it once it computes

cacheSolve <- function(x, ...) {
	iX <- x$getinverse()
	if(!is.null(iX)) {
		message("getting cached inverse matrix")
		return(iX)
	}
	mat <- x$get()
	iX <- solve(mat, ...)
	x$setinverse(iX)
	iX
      ## Return a matrix that is the inverse of 'x'
}
