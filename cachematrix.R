## The 'cachematrix.R' illustrate caching the inverse of a matrix, wich it contains two functions:
## The first function 'makeCacheMatrix()' are used to create a special object that stores a matrix
## The second fuction 'cacheSolve()' caches its inverse.


## The first function, 'makeCacheMatrix()', creates a special matrix, compute its inverse and stored 
## these values. It is really a list with four funcions: first to set the value of the matrix, second to get 
## the value of the matrix, third to set the value of its inverse and finally to get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize objects for the inverse property
    ## 'x' is a function argument, wich its default value is an empty matrix
    ## 'm' is an object within the 'makeCacheMatrix()' environment
    m <- NULL 
    
    ## The 'set()' function sets the matrix taking an argument that is named as 'y'
    set <- function(y){
        ## The values of 'x' and 'm' are assigned in the parent environment
        x <<- y
        m <<- NULL 
    }
    
    ## The 'get()' function defines the getter for the matrix 'x'
    get <- function(){
        ## Return the matrix
        x
    }
    
    ## The 'setInverse()' function defines the setter for the inverse of the matrix 'm'
    setInverse <- function(inverse){
        ## 'm' is defined in the parent environment
        m <<- inverse
    }
    
    ## The 'getInverse()' function defines the getter for the inverse matrix of 'm'
    getInverse <- function() {
        ##Return the inverse property
        m
    }
    
    ## Each of these functions are assigned as an element within a list(), 
    ## and returns it to the parent environment
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The second function, 'cacheSolve()', compute the inverse of the special matrix returned by 
## 'makeCacheMatrix()'. If the inverse has already been calculated (and the matrix has not changed),
## then the 'cacheSolve()' should retrieve the inverse from the cache stored. 

cacheSolve <- function(x, ...) {
    ## The function stars with the argument 'x' 
    ## and an ellipsis that allows pass additional arguments into de function
    
    ## It attempts to retrieve the matrix inverse from 'getInverse' function
    m <- x$getInverse()
    
    ## It checks whether the result was already set to return the inverse
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## If the result of '!is.null(m)' is FALSE, 
    ## 'cacheSolve()' get the matrix from the input object
    data <- x$get()
    
    ## It calculates the inverse matrix
    m <- solve(data, ...)
    
    ## It used the 'setInverse()' function on the input object to set its inverse matrix
    x$setInverse(m)
    
    ## Return the value of the inverse matrix to the parent environment by printing it.
    m
}