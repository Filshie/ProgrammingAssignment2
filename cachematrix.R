## Programming Assignment 2

## These functions will find the inverse of a specified matrix (using Solve). 
## If the inverse has already been calculated then will display a message and 
## return the cached result instead to save on precious computational time


## Create a list containing functions to set/get the matrix value 
## and set/get the inverse value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## Takes returned matrix from makecCacheMatrix and checks if we've already 
## calculated the inverse, if we have then return the cached result instead

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("found cached data, grabbing!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}

## Results from testing out the functions using a simple matrix

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
