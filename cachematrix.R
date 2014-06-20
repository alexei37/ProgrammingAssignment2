## Below are two functions that are used to create a special object 
## that stores a numerical matrix and cache's its inverse.

### makeCacheMatrix() creates a special "vector",
### which is really a list containing four functionS:
### 1. Set the value of the matrix;
### 2. Get the value of the matrix;
### 3. Set the value of the inverse of the matrix;
### 4. Get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  # The placeholder for the inverse
  inverse <- NULL
  # Function to set the value of the matrix:
  set <- function(y) {
    x <<- y
    inverse <<- NULL # Initialize the "global" (cached) inverse
  }
  # Function to get the value of the matrix:
  get <- function() x
  # Function to set the value of the inverse of the matrix:
  set_inverse <- function(inv) inverse <<- inv
  # Function to get the value of the inverse of the matrix:
  get_inverse <- function() inverse
  # Create a list of these four functions (and return it)
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


### Returns a matrix that is the inverse of 'x'.
### Uses cache result whenever available.
cacheSolve <- function(x, ...) {
  # Try cached result:
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  # If cached result is not available, get the data...
  data <- x$get()
  # ... compute the inverse ...
  inverse <- solve(data, ...)
  # ... and store the result in the cache.
  x$set_inverse(inverse)
  # Return the inverse
  inverse
}
