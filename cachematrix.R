## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# makeCacheMatrix create an R object which calculates the matrix^-1 only once

makeCacheMatrix <- function(x = matrix()) {
   ix  <- NULL
   set <- function(mx) {
        x <<-  mx
        ix <<- NULL
   }
   get <- function(){
        x
   }
   setinverse <- function(inverse) {
        ix <<- inverse
   }
   getinverse <- function() {
        ix
   }
   list(set = set
        , get = get
        , setinverse = setinverse
        , getinverse = getinverse)
}


# returns the inverse matrix from the object obtained from 
# makeCacheMatrix calculating the value only once
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                return(i)
        }
        mx <- x$get()
        i  <- solve(mx, ...)
        x$setinverse(i)
        i
}

