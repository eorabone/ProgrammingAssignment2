## This set of 2 functions will cache the inverse of a matrix 

## The "makeCacheMatrix" function creates a special matrix object
## that will cache the inverse of a matrix, by setting and 
## getting its value and then setting and getting its inverse 
## using "solve"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## The "cacheSolve" function computes the inverse of the 
## special matrix returned by the "makeCacheMatrix" function, 
## but will return the inverse from the cache if the inverse
## has already been calculated

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data1 <- x$get()
        m <- solve(data1, ...)
        x$setmatrix(m)
        m
}