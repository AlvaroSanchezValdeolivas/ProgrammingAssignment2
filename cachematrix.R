## The aim of the next two functions is to cache the inverse of a matrix. For
## that purpose, the first function creates a list that later is used as
## argument in the second one.

## "makeCacheMatrix" function creates a list containing the fields with the 
## funcions: "set" (stores the matrix introduced as argument); "get"
## (returns the matrix x); "setinv" (stores the inverse of the matrix x);
## and "getint" (returns the  inverse of x).

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## "cacheSolve" funtion computes the calculation of the inverse of the matrix
## stored with the "makeCacheMatrix" function and stores it, only if "getinv"
## returns null. In the contrary, it returns the value stored with the
## "makeCacheMatrix" and avoids its computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i    
    
}
