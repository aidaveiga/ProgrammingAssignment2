 ## Matrix inversion is usually a costly computation and so the functions below  
 ## will cach the inverse of a matrix rather than compute it if it has already been
 ## calculated.


## makeCacheMatrix is a function that caches the inverse of a matrix passed as an
## argument to the function. It also creates a list of functions that gets/sets the
## matrix passsed as argument and gets/sets the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(invmvalue) invm <<- invmvalue
    getinvmatrix <- function() invm
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
    }



## cacheSolve function calculates the inverse matrix if it has not yet been
## calculated and sets its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvmatrix()
    if(!is.null(invm)) {
      message("getting inverse matrix cached data")
      return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvmatrix(invm)
    invm
}
