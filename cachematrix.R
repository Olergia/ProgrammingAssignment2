
## Example of use:
## > 	x <- matrix(rnorm(100, 24, 10), 10, 10)
## > 	y <- makeCacheMatrix(x)
## > 	cacheSolve(y)


##  The makeCacheMatrix constructs an object(list) with getting/setting methods for access to global variables

makeCacheMatrix <- function(x = matrix()) {
	
	## global variable for storing the inverse matrix
	inv <- NULL

	## function for storing original matrix
	set <- function(y) {
		## store original matrix in parent environment
		x <<- y
		##  reset value of the inverse matrix
		inv<<- NULL
	}

	## function for retrieving original matrix
	get <- function() x

	## function for storing the inverse matrix
	setInverse <- function( inverse) inv <<- inverse

	## function for retrieving inverse matrix
	getInverse <- function() inv

	## list of methods 
	list( set = set, get = get,
	      setInverse = setInverse,
	      getInverse = getInverse)
}


## the cacheSolve returns inverse matrix from cache or calculates it

cacheSolve <- function(x, ...) {

	inv <- x$getInverse()
	if( !is.null(inv)) {
		message("Getting cached inverse matrix")
		return(inv)
	}

	## There is no inverse matrix yet. Calculate it. 
	data <- x$get()
	inv <- solve(data, ...)

	## Cache the inverse matrix
	x$setInverse(inv)
	inv
}
