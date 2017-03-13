## makeCacheMatrix creates a list object that cache the inverse of its input argument matrix x
## cacheSolve computes the inverse of the list object returned by makeCacheMatrix. If the inverse
## has been cahced, the function returns the cached inverse. If not, the function compute
## the inverse and returns it.

## The "makeCacheMatrix" takes in a matrix argument x and return a list of four functions
## The "set" function takes a new matrix y and set x's value to y and reset i's value to NULL
## The "get" function takes no input argument and returns the matrix x
## The "setinv" function takes a inverted matrix and set it to i
## The "getinv" function takes no input argument and returns the inverted matrix i

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The "cacheSolve" function takes a input argument matrix x and check the cache wether the 
## inverse of the matrix has been saved, if yes, it will take the inverse from the cache and 
## return it, if no, it will calcuate the inverse and return it

## First, the function checks wether i is NULL, if i is not NULL, 
## it means the invert has been calculated, and the function just returns the saved i
## Second, if i is NULL, it means the invert has not been calculated, then the function gets
## the original matrix x and pass it to the matrix data
## Third, calculate the invert of data and assign it to i 
## Fourth, set the inverted matrix to i in the cache
## Lastly, return the inverted matrix


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
