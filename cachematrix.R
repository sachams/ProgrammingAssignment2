## This file contains a pair of functions which allow matrix inverses to be cached, increasing performance
## when the cached inverse is accessed frequently.

## makeCacheMatrix
## If pased in a matrix, it returns an object which allows a cached inverse to be stored alongside the
## original. The contents can be accessed using the following methods:
## 
## set - sets a new matrix
## get - returns the matrix
## getinverse - returns the inverse of the matrix which has been previouslt calcualted and stored
## setinverse - sets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	
	## Clear out any previously-cached inverse
	i <- NULL
	
	## Create a setter function
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Create a getter function
	get <- function() x
	
	## Create functions to get/set the inverse
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	## Return a list containis getters and setters
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## cacheSolve
## If passed an object of the type returned by makeCacheMatrix, it returns the inverse of the matrix.
## If the inverse has previously been calculated it returns the cached version. If not, it calculates
## the inverse then caches the result then returns it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	## Check to see if the stored inverse is null
	i <- x$getinverse()
	if(!is.null(i)) {
		## it is not null, so return that
		message("getting cached data")
		return(i)
	}
	
	## it is null, so calculate it now and store it
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i 
}
