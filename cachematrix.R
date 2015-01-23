## These functions for computing the inverse of a matrix.
## They can cache the inverse of a matrix rather than compute it repeatedly. 

##  The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
		setm <- function (y) {
			x <<- y
			invm <<- NULL
		}
        getm <- function() {x}
        setinvm <- function(solve) {invm <<- solve}
        getinvm <- function() {invm}
        list(setm = setm, getm = getm,
             setinvm = setinvm,
             getinvm = getinvm)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the "cacheSolve" retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting the cached inverse matrix")
        return(invm)
        }
    data <- x$getm()
    invm <- solve(data)
	x$setinvm(invm)
    invm
}
