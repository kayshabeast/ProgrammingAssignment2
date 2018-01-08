## Two functions that yield the inverse of a matrix, x.
## makeCacheMatrix converts a matrix into a format where it will work well with
## solve() and then cacheSolve uses solve() to create an inverse matrix.

## Function 1: Creates a matrix that is designed to work well with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
## prepares a home for the inverse matrix
inv <- NULL
	
## Setter and getter for original matrix
set <- function(y) {
	x <<- y
	inv <<- NULL
	}

get <- function() x
	
## Setter and getter for inverse matrix
setinv <- function(solveMatrix) inv <<- solveMatrix
getinv <- function() inv
	
## List of functions
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function 2: Caches the inverse of a matrix created using the makeCacheMatrix function.
## Also checks to make sure that the original matrix hasn't already been inverted so
## that the result is always an inverted version of the original, not an inversion
## of the inversion.

## Function takes the name of the matrix (x) and extra arguments (...)
cacheSolve <- function(x, ...) {

	inv <- x$getinv()

	## Checks to see if the matrix is already inverted, and if so, returns it.
	if(!isnull(inv)){
		message("Inversion is already complete! Ta da!")
		return(inv)
	}

## Fetches the data needed to invert the matrix
data <- x$get()

## Computes the matrix's inversion using solve()
inv <- solve(data)

## Caches the inverse that was just computed
x$setinv(inv)

## Prints the inverted matrix
return(inv)
}
