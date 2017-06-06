## These functions allow the inverse of a matrix to be cached for faster
## retreval than calculating.  The functions work in tandem, where 
## makeCacheMatrix creates an initial set of functions and variables which R 
## holds in memory and cacheSolve uses those functions to set/get the 
## initial or inverted matrix to check if there's an inverse already and if not
## it calculates it.

## Creates a list of functions in memory used to set/get the initial/inverted 
## matrix. inv stores the inverse, y is the matrix passed in which is
## stored in x, inverse is passed in from cacheSolve, naming the functions in
## a list allows for $ subsetting.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Checks for an inverted matrix in memory, calculates one if none found, and 
## puts the inverse in memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
