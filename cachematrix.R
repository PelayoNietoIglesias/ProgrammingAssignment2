## The makeCacheMatrix and cacheSolve functions in R optimize matrix inversion
## by caching the result. Instead of recalculating the inverse every time,
## makeCacheMatrix stores the matrix and its inverse, while cacheSolve retrieves
## the cached inverse if available. This improves performance in repeated
## calculations involving the same matrix

## The function makeCacheMatrix is useful to store the inverse matrix of
## a given matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inversa) inv <<- inversa
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function allows to get the inverse of the previous matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Obteniendo inversa de la caché")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
