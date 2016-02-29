## These are two functions I wrote for the 2nd programming assignment
## of the R Programming Coursera course. The first funciton creates a
## special "matrix" object that can cache its inverse. The second 
## function computes the inverse the inverse of the special matrix
## that returned by makeCacheMatrix.

## Basically, makeCacheMatrix is a function that is a list of 
## functions. It stores a matrix and its inverse.

makeCacheMatrix <- function(x = numeric()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_matrix <<- solve
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This funciton calculates the inverse of the matrix that is set in
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
