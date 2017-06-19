## Put comments here that give an overall description of what your
## functions do

## creates a matrix using 4 functions to put values into the martix and store the inverser in cache

makeCacheMatrix <- function(x = matrix()) {
  #n is set to missing value
  n <- NULL 
  
  #put values in the matrix  
  put_values <- function(p) 
    {
    x <<- p
    n <<- NULL
  }
  
  #gets the value assigned to the matrix
  get_values <- function() x 
  
  #makes the inverse value of the matrix
  Make_inverse <- function(inverse) n <<- inverse 
  
  #gets in the inverse value of the matrix
  get_inverse_values <- function() n 
  list(put_values = put_values,
       get_values = get_values,
       Make_inverse = Make_inverse,
       get_inverse_values = get_inverse_values)

}


## calculates the inverse of matrix x1

cacheSolve <- function(x, ...) 
  {
  n <- x$get_inverse_values()
  if (!is.null(n)) {
      message ("fetching cached values")
      return(n)
  }
  
  values <-x$get_values()
  n <- solve(values, ...)
  x$Make_inverse(n)
  n
}

x <- matrix(c(1,2,3,4),2,2)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
