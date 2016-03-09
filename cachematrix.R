## Functions in this file help create and cache a matrix and its inverse
## so that one does not need to calculate matrix inverse if the matrix
## has not changed and a matrix inverse is present

## makeCacheMatrix function: Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  create_matrix <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  
  get_matrix <- function() x
  
  set_matrix_inv <- function(inv) inv_matrix <<- inv
  get_matrix_inv <- function() inv_matrix
  
  list(create_matrix = create_matrix, get_matrix = get_matrix, 
       set_matrix_inv = set_matrix_inv, get_matrix_inv = get_matrix_inv)
}


## cacheSolve Function: Takes the output matrix from makeCacheMatrix function
## and retrieves the inverse of the matrix from its cache, if not, calculate it 
## and store it in the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_matrix_inv()
  if(!is.null(inv_matrix)){
    message("Fetching Cached Matrix Inverse")
    return(inv_matrix)
  }
  
  spec_matrix <- x$get_matrix()
  inv_matrix <- solve(spec_matrix)
  x$set_matrix_inv(inv_matrix)
  inv_matrix
    
}
