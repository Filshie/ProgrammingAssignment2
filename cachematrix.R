## Programming Assignment 2

## These functions will find the inverse of a specified matrix (using Solve). 
## If the inverse has already been calculated then display a message and return
## the cached result instead. Saves on precious computational time!


## Funtion to create a special matrix object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Takes returned matrix from makecCacheMatrix and checks if we've already 
## calculated the inverse, if we have then return the cached result instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("found cached data, grabbing!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Testing out the functions using a simple matrix

## > my_matrix <- matrix(c(-1, 0, 2, 4, 6, 8, 10, 12, 14), 3, 3)
## > my_matrix
## [,1] [,2] [,3]
## [1,]   -1    4   10
## [2,]    0    6   12
## [3,]    2    8   14
## > cache_matrix <- makeCacheMatrix(my_matrix)
## > cacheSolve(cache_matrix)
## [,1]      [,2] [,3]
## [1,]    1 -2.000000  1.0
## [2,]   -2  2.833333 -1.0
## [3,]    1 -1.333333  0.5
## > cacheSolve(cache_matrix)
## found cached data, grabbing!
##         [,1]      [,2] [,3]
## [1,]    1 -2.000000  1.0
## [2,]   -2  2.833333 -1.0
## [3,]    1 -1.333333  0.5
