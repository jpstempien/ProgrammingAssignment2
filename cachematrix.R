## makeCacheMatrix creates a special "matrix" that includes a
## list of functions. It takes in a matrix as an argument and
## assignes a list of special functions: set; get; setinverse; getinverse
## set is used in case you want to change the ocntents of your matrix
## get is used to retrive the contents of the matrix
## setinverse is used to set the inverse
## getinverse is used to retrive the inverse
## cacheSolve is a function that checks if the inverse exists and
## if it does it returns it, otherwise it computes it and stores in previous
## function

## Below function gets the matrix and allows to modify its contents,
## display contents, store inverse of the matrix, get the inverse

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL
        set<- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}
## Below function checks if the inverse of the matrix is stored
## in the above function and in case it isn't, it calculates it and 
## stores it inside previous function
cacheSolve <- function(x, ...) {       
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}