## My functions store the matrix supplied with its inverse, and then I can
## return the inverse from either cached data or solve the inverse.



## This function, "makeCacheMatrix", produces a list of functions that can
## do the followings:
## 1) get the value of the matrix;
## 2) set the value of the matrix;
## 3) get the value of its inverse;
## 4) set the value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        setmatrix <- function(y){
            x <<- y
            Inv <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list( setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
              getinverse = getinverse)
   
}


## "CacheSolve" is a function that generates the inverse of a given matrix.
## If it is stored in the cached data, then I output the inverse stored;
## otherwise I compute the inverse and make output.

cacheSolve <- function(x, ...) {
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data...")
        return(Inv)
    }
    data <- x$getmatrix()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
    Inv
    
}
