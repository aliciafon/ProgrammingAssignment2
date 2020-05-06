## The function "makeCacheMatrix" produces a unique matrix which has the ability
## to cache its inverse.While the "cacheSolve" function is responsible for 
## computing the unique matrix when produced from the afromentioned function, 
## "makeCacheMatrix".  

## The purpose of the following two funtions is to cache the inverse of a matrix. 



makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              set <- function(y){
                x <<- y
              inv <<- NULL
              
              }
              get <- function()x
              setinverse <- function(inverse) inv <<- inverse
              getinverse <- function() inv
              list(set = set,
                   get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
}        


## This funtion will return the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  
        ## The inverse of 'x' will be returned from the original matrix.
  
                  inv <- x$getinverse()
                  if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
                    
                  }
                  
                  data <- x$get()
                  inv <- solve(data, ...)
                  x$setinverse(inv)
                  inv    
  
  }

