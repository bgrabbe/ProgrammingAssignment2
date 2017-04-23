## Matrix inversion can be very computationally resource intensive.
## To avoid needless re-computation the inverse of a matrix
## should only be worked out once.
## So the first time we work it out we cache the result.
## The next time the result is asked for, if it has already
## been worked out as is sat in the environment, we use it.

# Create the enclosing environment
# in order to cache the matrix.
# The makeCacheMatrix object will consist of
# four functions encapsulated in a list
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  # Initialise 'inv' (i.e. martix variable) to NULL, then will be set on input 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## The cacheSolve function will check and see if the 
## inverse has already been cached and if so return it with a message,
## or calculate it, cache it, and then return the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  ## does it exist in the cache ??
  if(!is.null(inv)) {
    # if so throw a message and return it		
    message("Getting cached matrix")
    return(inv)
  }
  ## else do the calculation to find the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # cache the result and return it
  x$setinverse(inv)
  inv    
}