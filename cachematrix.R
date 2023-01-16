## Matrix inversion is sometimes an expensive process, so saving the inverse of a matrix rather than computing it repeatedly may have some advantages.
## Here are two functions for making a unique object that caches the inverse of a matrix and stores it.
## The "matrix" object created by this function is a unique one that can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL}
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The inverse of the unique "matrix" produced by makeCacheMatrix is calculated by this function. 
## The inverse should be retrieved from the cache if it has previously been calculated .

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)}
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
