### These two functions implement a simple cacheing system to save
### repeated computations of the matrix inverse.

### makeCacheMatrix returns a list with getter and setter functions for
### the matrix, and also getter and setter for the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setSolve <- function(i) inverse <<- i
    getSolve <- function() inverse
    list(get=get, set=set, getSolve=getSolve, setSolve=setSolve)
}

### cacheSolve looks up or computes the inverse of the cacheMatrix
### 'x', storing it if necessary.
cacheSolve <- function(x, ...) {
    i <- x$getSolve()
    if(!is.null(i)) return(i) # we're done
    m <- x$get()
    i <- solve(m)
    x$setSolve(i) # store the inverse...
    i # ...and return
}
