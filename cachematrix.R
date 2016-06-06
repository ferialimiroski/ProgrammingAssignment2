## This function creates a special "matrix" object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  inverse <- NULL
        setvalue <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getvalue <- function() x
        setInverse <- function(inverse) inverse <<- inverse
        getInverse <- function() inverse
        list(setvalue = setvalue,
             getvalue = getvalue,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix_a <- x$getvalue()
        inverse <- solve(matrix_a, ...)
        x$setInverse(inverse)
        inverse


}
