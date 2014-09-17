## This functions create a matrix,calculate and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix<-NULL
        set<- function (y ){
                  x <<- y
                  invmatrix <<- NULL
        }
        get<-function() x
        setinverse <- function(solvedmatrix) invmatrix<<-solvedmatrix
        getinverse <- function() invmatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix<-x$getinverse()
        if (!is.null(invmatrix)) {
                message("Getting cached data...")
                return(invmatrix)
        }
       data<- x$get() 
       invmatrix <- solve(data, ...)
       x$setinverse(invmatrix)
       invmatrix
}
