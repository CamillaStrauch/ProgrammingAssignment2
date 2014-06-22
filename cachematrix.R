## This file have been updated as part of a course assignment
##
## The purpose of the written functions is to illustrate the benefits of the scoping rules of the R  
## as a means to cache potentially time-comsuming computations
##
## 1) The makeCacheMatrix-function:
##    ------------------------------
##    This function creates a special object that can cache the inverse of a matrix
##    It is implicitly assumed that the matrix is invertible and that the inverse can be found via the solve -function :
##        i) The determinant of the matrix should differ from zero ( det(x)!= 0 )
##       ii) The matrix is square 
##
##    The makeCacheMatrix function is analogous to makeVector-function in the course example : 
##    - the solve-function is used instead of the mean-function  

makeCacheMatrix <- function(x = matrix()) {
 
     ## initiate the inverse matrix object
        i <- NULL

     ## set the value of the matrix 
        set <- function(y) {
               x <<- y
               i <<- NULL
        }

     ## get the value of the matrix
        get <- function() x

     ## set the value of the inverse matrix
        setinverse <- function(solve) i <<- solve

     ## get the value of the inverse matrix
        getinverse <- function() i

     ## 
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## 2) The cacheSolve-function:
##    -------------------------
##    This function computes the inverse of the special matrix returned by makeCacheMatrix
##
##    The cacheSolve-function is analogous to cachemean-function in the course example  


cacheSolve <- function(x=matrix(), ...) {
       i <- x$getinverse()

      ## return the cached inverse matrix - if it exists
       if(!is.null(i)) {
              message("getting cached data")
              return(i)
       }

      ## otherwise calculate the inverse matrix
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
     
     ## Return a matrix that is the inverse of 'x'
     i  
}
