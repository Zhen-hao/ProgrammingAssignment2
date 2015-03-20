## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
## The function makeCacheMatrix takes a matrix x as the argument, 
## the defaut value of which is an empty 1 by 1 matrix     
    
    ## y is the potential inverse of x
    inv <- NULL
    
    ## set is the function to set the value of the matrix, x
    set <- function(y) {
        x <<- y
        ## After reseting x, its inverse inv should be set to NULL
        inv <<- NULL
    }
    
    ## get is the function that get the value of the matrix, x
    get <- function() x
    
    ## setinv is the function that set the value of the inverse, inv 
    setinv <- function(inverse) inv <<- inverse
    
    ## getinv is the function that get the value of the inverse, inv
    getinv <- function() inv
    
    ## list the returned value of the function makeCacheMatrix,
    ## which is the list of functions to
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse
    ## get the value of the inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The following function cacheSolve calculates the inverse of the special "vector" 
## created with the above function makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean 
## in the cache via the setinv function

cacheSolve <- function(x, ...) {
    
    ## get the inverse of the matrix of x by calling the function getinv in the list x
    inv <- x$getinv()
    ## check if inv has already been calculated. If so, return the value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if inv has not been calculated,
    ## read the matrix of x into m
    m <- x$get()
    ## caculate the inverse of m and save to inverse
    inverse <- solve(m, ...)
    ## set the value of inverse in x with inverse by using the setinv function in x
    x$setinv(inverse)
    ## return the inverse of the matrix
    inverse
    
}
