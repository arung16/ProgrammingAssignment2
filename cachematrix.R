## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set: sets the matrix
## get: gets the matric
## setinverse: sets the inverse of the matrix
## getinverse: gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## Checks if the inverse matrix is cached and returns the cached matrix if it exists. If not, it returns the inverse of
## the matrix using the solve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Cached data available")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
