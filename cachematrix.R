## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  getinverse <- function(){
    inv
  } 
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
    x1<-makeCacheMatrix(x)
    inverse <- x1$getinverse()
    
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    data <- x1$get()
  
    inverse <- solve(data)
  
    x1$setinverse(inverse)
  
    inverse
}

makeCacheMatrix(z)