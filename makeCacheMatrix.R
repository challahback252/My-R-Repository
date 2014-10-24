## Complementary functions makeCacheMatrix and cacheSolve
## These take in a matrix and provide the ability to store
## and return its inverse while only computing it once

## Creates a list of functions that operate around a matrix
## and its inverse. "Set" defines the matrix, "Get" 
## returns the matrix, "SetInv" stores the matrix's
## inverse (computed elsewhere), and "GetInv" returns
## the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Takes in a list created by makeCacheMatrix and
## computes the inverse of the matrix contained in it
## (unless it was already computed) and stores it
## via the "SetInv" function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
        	message("getting cached inverse")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
