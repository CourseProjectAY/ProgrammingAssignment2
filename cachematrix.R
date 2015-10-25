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