# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   
    # Following the same format as the assignment example
    # Creating a makeCacheMatrix object will consist of
    # four functions encapsulated in a list
    # 1. set the matrix
    # 2. get the matrix
    # 3. set the inverse of the matrix
    # 4. get the inverse of the matrix
    
    # Set the inverse to NULL
    inv <- NULL
    
    # Set the matrix itself but not the inverse
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    # Get the matrix itself but not the inverse
    get <- function() x
    
    # Manually set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse <- function() inv
    
    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # Get the current state of the inverse and see if it
  # has been computed yet
  inv <- x$getinverse()
  
  # If it has...
  if(!is.null(inv)) {
    # Simply return the computed inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If it hasn't...
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}
