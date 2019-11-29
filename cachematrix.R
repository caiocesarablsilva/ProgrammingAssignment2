####################
##### SOLUTION #####
####################

#From the example, we could try to make some adjusts:
#1 - rename the function to makeCacheMatrix
#2 - the function has x as a matrix now
#3 - the m (mean) was the special vector. Now we will replace to inverse matrix (inv_ma)
#4 - we are thinking about the inverse matrix and try to solve it like the example
#5 - make the others adjustments

## About this function: it creates a matrix object that can cache its inverse like the example about m (mean).

makeCacheMatrix <- function(x = matrix()) {
        inv_ma <- NULL
        set <- function(y) {
                x <<- y
                inv_ma <<- NULL
        }
        get <- function() x
        setinv_ma <- function(matrix) inv_ma <<- matrix
        getinv_ma <- function() inv_ma
        list(set = set, get = get,
             setinv_ma = setinv_ma,
             getinv_ma = getinv_ma)
}

## About this function: it comutes the inverse of the matrix 

cacheSolve <- function(x, ...) {
        inv_ma <- x$getinv_ma()
        if(!is.null(inv_ma)) {
                message("getting cached data")
                return(inv_ma)
        }
        data <- x$get()
        inv_ma <- solve(data, ...)
        x$setinv_ma(inv_ma)
        inv_ma
}
