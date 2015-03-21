## The below fuctions calculate and cache the inverse of a matrix.

## makeCacheMatrix outputs a list of functions that store and retrieve a matrix x and 
## store and retrieve its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the inverse of a matrix stored using the makeCacheMatrix fuction.
## First, it will check to see if the inverse has already been calculated and stored.
## If it has, it will return the cached matrix. If not, it will calculate the inverse, 
## cache it, and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
