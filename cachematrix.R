## These are a pair of functions that
## cache the inverse of a matrix

##  The first function creates a special "matrix" object that can cache its inverse, 
## it contains the function to :

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Set the value of the matrix        
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        ## get the value of matrix
        get <- function() x
        ## set the value of the inverse of the matrix
        setinver <- function(inverse) inver <<- inverse
        ## get the value of the invers of the matrix
        getcache <- function() inver
        list(set = set, get = get, setinver = setinver,getcache = getcache)

}


## The following function return the inverse of the matrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the result from the
## cache and skips the computation. Otherwise, it calculates data and sets the results in the cache via the `setinver`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getcache()
        if(!is.null(getcache)) {
                message("getting cached data")
                return(getcache)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}
