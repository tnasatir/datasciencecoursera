# Creates a matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  #create the matirx inverse object and set it to null
  inv <- NULL
  
  #set the matrix value and clear cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #Set inverse manually
  setinverse <- function(inverse) inv <<- inverse
  
  #get the inverse
  getinverse <- function() inv
  
  #create list
  list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


# Computes the inverse of the matrix returned by makeCacheMatrix above,
# or retrieves the cached matrix. 

cacheSolve <- function(x, ...) {
  
  # Check to see if the inverse exists in the cache.
  inv <- x$getinverse()
  
  if (!is.null(inv))  {
    message("Getting cached data")
    return(inv)
    }
  
  # If the matrix does not exist in the cache yet....
  # Get the matrix data
  data <- x$get()
  
  # Calculate and return the inverse
  inv <- solve(data)
  return(inv)
  
}
