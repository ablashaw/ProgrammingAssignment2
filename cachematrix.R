# makeCacheMatrix and cacheSolve are functions designed to compute the inverse
# of an input matrix and/or quickly return the inverse matrix, avoiding
# unncessary computations


makeCacheMatrix <- function(x = matrix()) {
    # creates a variable 'x' containing a list whose elements are functions that
    # define a matrix, return the input matrix values, calculate its inverse,
    # and retrive the inverse matrix from "cached" memory
    
    m <- NULL  # initiate variable m to store result from matrix solve()
    set <- function(y) {
        x <<- y  # assign initial matrix values
        m <<- NULL  # reset inverse matrix for new input matrix 'x'
    }
    get <- function() {
        x  # return input matrix 'x'
    }
    setsolve <- function(xsolve) {
        m <<- xsolve  # store inverse matrix result as 'm'
    }
    getsolve <- function() {
        m  # return inverse matrix result, if available, or NULL
    }
    
    # return a list with functions as elements of the list
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
    # determines if the inverse of matrix 'x' is in memory and calculates it if
    # unknown, then returns the matrix inverse of 'x'
    
    m <- x$getsolve()  # retrieving inverse matrix 'm'
    
    if (!is.null(m)) {  # checking if inverse has already been calculated
        message("getting cached data")  # inverse is already in memory
        return(m)  # returning cached result for inverse
    }
    
    data <- x$get()  # retriving matrix 'x'
    m <- solve(data, ...)  # calculating inverse for matrix 'x'
    x$setsolve(m)  # caching inverse matrix
    m
}