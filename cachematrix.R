##The goal of these functions is to cache the inverse of a matrix passed to the functions


##The first function "makeCacheMatrix" will create a special matrix object with a list which 
##will be used to cache the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}




##The second function "cacheSolve" will compute the inverse of the matrix created in the 
##first function "makeCacheMatrix".  If the inverse has already been calculated for the 
##passed matrix in question, then it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
        ## Return a matrix that is the inverse of 'x'

