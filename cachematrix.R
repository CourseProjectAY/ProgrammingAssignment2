## This function - makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) { ## This sets the value of the matrix
                x <<- y
                inver <<- NULL
        }
        get <- function() x ## This gets the value of the matrix
        setinverse <- function(inverse) inver <<- inverse ## This sets the inverse of the matrix
        getinverse <- function() inver   ## This gets the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. if the inverse has already been calculated and if the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache

## The function calculates the inverse of the special "matrix" created with the 
## above function makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. if so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache
##  via the setinverse function.

cachesolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) { ## Here the function checks if the inverse has been calculated or not
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...) ## Here the function calculates the inverse of the matrix if required
        x$setinverse(inver)
        inver
}