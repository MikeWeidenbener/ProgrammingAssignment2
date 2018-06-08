## These two functions will create a matrix that will store an inverse.
## The inverse can then be retrieved as needed without requiring another call
## to the solve function

## this is the creator for a CacheMatrix object.  It will take the matrix
## as a parameter store the matrix and inverse in the parent environment.

makeCacheMatrix <- function(x=matrix()) {
	inv<- NULL
	set <- function(y) {
	## store the matrix in the parent environment.
		x <<- y
	## clear out any existing values for the inverse
		inv <<- NULL
	}

	## retrieve the value for x from the parent environment
	get <- function() x

	## store the inverse into the parent environment
	setinverse <- function(inv) inv  <<- inv

	## retrieve the inverse from the parent environment
	getinverse <- function() inv

	## create a named list of the functions so they can be referenced by name
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

## this is the function to retrieve or update and retrieve the inverse
## of the matrix

cachesolve <- function(x, ...) {

	## get the inverse from the parent environment
	inv <- x$getinverse()

	## if the inverse has been calculated, return it and exit
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	## if the inverse is null, calculate it using solve
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)

	## return the newly calculated inverse
	inv
}



