## First we create a matrix with makeCacheMatrix(), which gives a list containing functions to
## set the matrix, get the matrix, set the inversed matrix, get the inversed matrix
## Then we call the cacheSolve() and this function will first check if s is NULL in the list
## If it is NULL, that means we have not inversed this matrix yet
## then we will do the solve() and assign the result to s
## so that the next time we run getSolve() we will get a result from previous calculation
## If s is NOT NULL, we can simply return s since we've already done the calculation before, no more calculation needed!

## This function forms a format of 'vector' that has addtional attributes to store calculation results in cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function check if the cache exists or it will do the calculation and store the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

## Test in console:
## > m <- makeCacheMatrix(matrix(rnorm(36), nrow=6))
## > cacheSolve(m)
