## The function creates a matrix that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
           set <- function(y){
                   x <<-y
                 inv <<-NULL
           }
           get <- function()x
           setinverse <-function(solveMatrix) inv <<-solveMatrix
           getinverse<-function()inv
           list(set=set, get=get, 
                setinverse=setinverse, 
                getinverse=getinverse)
 }

## The function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)){
               message(“getting cached data")
               return(inv)
       }
       data <-x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
 }
