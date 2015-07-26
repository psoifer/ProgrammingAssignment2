
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  #saves the inverse matrix
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  #returns the inverse matrix
  getinverse <- function(){
    inv
  } 
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
  
  #should cache object to use it in the future
  if(is.null(x1)) {
    #chaching only in case that the object doesn't exist
    x1<<-makeCacheMatrix(x)
  }
    inverse <- x1$getinverse()
    
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    data <- x1$get()
  
  
    
    inverse <- solve(data)
    
  #updates the cached object
  
    x1$setinverse(inverse)
  
    inverse
}

