# Assignment: R Programming Assignment 2: Lexical Scoping

# makeCacheMatrix takes a matrix and sets variables and functions in memory, 
# and then returns a list of functions in makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                  # Initialize the local m to NULL so we know when cacheSolve has run at least once
  set <- function(y) {                       # Create a set function to store the matrix passed in the call as x and NULL as m, both in cache
    cache_x <<- y                            # Pass the initial matrix from the command line into cache as cache_x
    cache_m <<- NULL                         # Initialize cache_m to NULL so we can tell when cacheSolve has run at least once
  }
  get <- function() cache_x                  # Create a function to get/return the matrix passed in the command line call to set
  set_cache_m <- function(m) cache_m <<- m   # Create a function to set the value of cache_m in cache to the value of m passed in the call to set_cache_m
  get_cache_m <- function() cache_m          # Create a function to retrieve value of cache_m from cache and return cache_m to check it for NULL
  list(set = set, 
       get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}


# cacheSolve function takes a matrix defined as makeCacheMatrix() and creates an invertible matrix using the m$set() function 
# nested in makeCacheMatrix().  cacheSolve returns the inverted matrix, first checking to see if a non-NULL value for m already
# exists in cache. If cacheSolve finds a non-NULL value for m existing in cache already, it returns that value.  Otherwise, 
# cacheSolve gets the command line values for m, inverts the matrix in m, and sets the value of m in the cache environment 
# to the inverted matrix that is created.

cacheSolve <- function(x,...) {
  m <- x$get_cache_m()              # Get the value for m in the cache environment and make it the local m
  if(!is.null(m)) {                 # Check to see if m is NULL
    message("getting cached data")  # If m is not NULL, return the value of m with the message "getting cached data"
    return(m)
  }
  start_matrix <- x$get()            # Call the function x$get in makeCacheMatrix to find the matrix with which to start, and assign it to start_matrix
  end_matrix <- solve(start_matrix)  # Use solve() to invert the start_matrix.  Assign the result to end_matrix
  x$set_cache_m(end_matrix)          # Call the nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in end_matrix
  end_matrix                         # Evaluate end_matrix and return if cache_m is non NULL.
}


# Test:
m <- makeCacheMatrix()
m$set(matrix(c(1,3,3,1), 2, 2))
m$get()
cacheSolve(m)
cacheSolve(m)



