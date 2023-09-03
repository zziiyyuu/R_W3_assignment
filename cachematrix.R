## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##name the matrix inverse as inv, inv is set as NULL at the beginning
        set <- function(y) {
                x<<- y
                inv <<- NULL
        }
        ## set a function to assign any value(y) to x
        ## this function also erase inv
        get <- function(){
                x
        }
        ## this function returns the value of x
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        ##this function cache the inverse of x
        getinverse <- function() {
                inv
        }
        ## this function returns the cached inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        ## define each function in this object
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## retrieve the cached inverse, inv may not have been calculated
        if(!is.null(inv)) {
                return(inv)
        }
        ## if inv is NOT NULL(calculated), then return inv
        data <- x$get()
        ## if inv is NULL(not calculated), then get the value of x
        inv <- solve(data)
        ## compute the inverse of x
        x$setinverse(inv)
        ## cache the inverse
        inv
        ## print the inverse
}
