## This function creates a "special" matrix that caches it's inverse. 

## The makeCacheMatrix function performs the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrx

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix. 
## It first checks if the inverse has already been computed. If so, it retrieves the inverse
## and skips the computation. If not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$getinverse(inv)
        inv
}
