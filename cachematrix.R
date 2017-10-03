#The following functions  are used to create a special object that stores a matrix and 
#cache's its inverse matrix

## The first function makeCacheMatrix creates a special vector which is really a list 
#containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse matrix
#4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve 
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

#The next function calculates the inverse of the special "vector" created with the above 
#function. However, it first checks to see if the inverse has already been calculated. If so,
#it gets the inverse from the cache and skips the computation. Otherwise, it calculates the 
#inverse of the matrix and sets the inverse matrix in the cache via the setmean function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
