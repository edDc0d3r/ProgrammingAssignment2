## Contains functions that calculate and cache the inverse of a matrix

## makeCacheMatrix - will create a special "matrix" object that can cache the inverse of a provided matrix
#

makeCacheMatrix <- function(x = matrix()) {
  
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
  
    get <- function() x
    setsolve <- function(solve) invm <<- solve
    getsolve <- function() invm
    
    #return a list of functions that will be referenced as x$
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve - if the inverse has already been calculated (and the matrix has not changed), then the 
#               cacheSolve function should retrieve the inverse from the cache; otherwise this function 
#               will compute the inverse of the special "matrix" returned by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## check if the inverse has already been calculated and return the value
        invm <- x$getsolve()
        if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
        }
        ## ...otherwise, R calculate and return the inverse of the matrix
        data <- x$get()
        invm <- solve(data, ...)
        x$setsolve(invm)
        invm
}

