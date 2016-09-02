#Week 3 Programming assignment: Caching the inverse of a Matrix

# Function 1: makeCacheMatrix function, fcn creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set=set, get=get, setinv =setinv, getinv=getinv)
}

# Problem 2: cacheSolve funciton, computes the inverse of the matrix made above, but if inverse already calculated then just retreives the inverse from the cache; either way, it returns a matrix that is the inverse of x

cacheSolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}