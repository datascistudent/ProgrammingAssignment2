##
## Assignment # 2
##

## Define the function which creates a "Matrix" of list of functions

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialization step
        i <- NULL
        
        ## Set the values of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the values of the matrix
        get <- function() x
                
        ## Set the inverse of the matrix
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        
        ## Get the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Get the inverse of the matrix        
        i <- x$getinv()
        
        ## Check if there is the matrix   
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If not, get the inverse of the matrix   
        data <- x$get()
        
        i <- solve(data, ...)
        
        ## Set the inverse of the matrix 
        x$setinv(i)
        i
}

##
## End of File
##
