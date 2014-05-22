## these are direct copies of the mean-based caching examples.  Tested with:
## matrix1 <- matrix(c(2,4,6,8,11,15,12,14,1,1,2,2,3,3,4,4),nrow=4,nrow=4)
## matrix1c <- makeCacheMatrix(matrix1)
## solve(matrix1)
## cacheSolve(matrix1c)
## (the above 2 were the same)

##makeCacheMatrix - this function created a special "matrix" object (really a list) that 
## can also store it's inverse/solve value.  (note Side effect is that we cannot perform solve directly
## against the makeCacheMatrix function-object/list.)  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

##cacheSolve - this function takes an function-object/list of makeCacheMatrix (or I suppose another
## that has it's same internal refernces, but it will not take an atomic vector or matrix) and runs
## the solve function against it, after first checking to see if the results are already cached, in 
## which case it returns the cached results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
      message("getting cached solution")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
