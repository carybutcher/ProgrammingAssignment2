## these two functions work together to assign and store the inverse of a square
## matrix and only call the solve function once if it is has already been solved

## returns a list of functions that allow the user to enter a square matrix through the set() function
## with the inverse available through the get() function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmat <- function(solve) m <<- solve
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}


## returns the inverse of a square matrix, retrieving it from the getmat() function if it has already
## been generated through the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmat(m)
        m
}
