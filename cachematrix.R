## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(mat) {
                x <<- mat
                inv <<- NULL
        }
        getMatrix <- function() x
        setInv <- function(mat) inv <<- mat
        getInv <- function() inv
       list(set = setMatrix, get = getMatrix,setinv = setInv,getinv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
