## Put comments here that give an overall description of what your
## functions do

### The two below functions cache the inverse of a matrix so that if we need it again,
### it can be looked up in the cache rather than recomputed.
### Otherwise, the inverse is computed and then stored for later use.


## Write a short comment describing this function

### Below first function creates a list of 4 sub-functions: set_matrix, get_matrix, set_inverse, get_inverse.

#* set_matrix function takes its argument and assigns it to a variable x in the parent environment. Then, it makes sure that
# the inverse is set to NULL.

#* get_matrix function takes the matrix stored

#* set_inverse function takes the inverse matrix (not computed here) and stores it in "inv"

#* get_inverse function takes the stored inverse matrix


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set_matrix <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get_matrix <- function() x
      
      set_inverse <- function(inverse) inv <<- inverse
      
      get_inverse <- function() inv
      
      list(set_matrix = set_matrix, 
           get_matrix = get_matrix,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## Write a short comment describing this function

### Below second function returns the inverse of a matrix.
### It first checks to see if the inverse has already been calculated. If so, it gets the inverse value 
### using get_inverse function as defined above. 
### Otherwise, it calculates the inverse matrix and sets this value in the cache thanks to set_inverse
### function as defined above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$get_inverse()
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      data <- x$get_matrix()
      inv <- solve(data, ...)
      x$set_inverse(inv)
      inv
}


## If you want to test it out:
# Initialize makeCacheMatrix()
# matrix <- makeCacheMatrix()
# Set the matrix
# matrix$set_matrix(matrix(1:9,3,3))
# Get the matrix 
# matrix$get_matrix()   
# Calculate the Inverse Matrix 
# cacheSolve(matrix)  
# If Inverse Matrix has already ben computed, then it returns the cached value 
# cacheSolve(matrix)                      
