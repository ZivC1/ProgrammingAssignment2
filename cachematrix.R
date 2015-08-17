
## This is a final working code for assignment 2. It was tested on a variety of
## test cases and worked perfectly fine.  



##  FUNCTION: makeCacheMatrix() ---------------------------------------------
# This function creates a special "matrix", which has several 
# functions (like methods) attached to it, including: 
# 1- setting the matrix, 2- getting the matrix,  3- setting the inverse matrix
# 4- getting the inverse matrix. 
## ------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
inv <- matrix()

  set <- function(y) {   # setting values to the matrix
  x <<- y
  inv <<- matrix()   ## set "blank" matrix in variable "inv"
  }
  
  get <- function() x  # displaying the matrix
  
  setinv <- function(inverse)  inv<<-inverse    # setting value to inverse matri
  
  getinv <- function() inv   # displaying the inverse matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv )
  
}
  ## FUNCTION: cacheSolve() ---------------------------------------------
  # This fuction calculates the inverse of a special "matrix" created with the
  # above fuction makeCacheMatrix.   It checks if an inverse matrix exists in 
  # cache. If yes - it outputs the cached matrix. If no - it claculates it.
  # Finally, it returns a matrix that is the inverse of 'x'
  ## ----------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        

    inv <- x$getinv()
    if (!is.na(inv[1][1])) { 
    message("getting cached data") 
    return(inv)
    }
    
    mat <-x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
