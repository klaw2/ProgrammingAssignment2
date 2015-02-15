## These functions calculate and store the inverse of a given matrix.
## Assuming no change in the matrix, this result can then be called 
## without having to recalculate the inverse every time.  

## This function creates and stores the matrix and enables caching.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks to see if the inverse of the matrix has already been
## calculated and stored. If it has, it returns the stored value. If it 
## hasn't, it will calculate the inverse and return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
