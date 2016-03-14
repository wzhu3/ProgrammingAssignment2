## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# use `<<-` to assign a value to an object in an environment 
# different from the current environment.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inv <<- inv  # assign inv matrix if existed
        getInv <- function() inv  # retrieve inv matrix
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)  #input for cacheSolve
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInv() # get the cache inverse matrix from object x
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()  #get the matrix
        inv <- solve(mat, ...) # cal the inverse
        x$setInv(inv)  #cache the inv
        inv            #return the inverse
}
