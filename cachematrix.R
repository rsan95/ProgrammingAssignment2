## Used to cache the inverse of a matrix based on the availability

## This function is used to cache the inverse of a matrix

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


## This function is used to assert whether the inverse is cached already and if no it computes and caches the inverse of a matrix

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
