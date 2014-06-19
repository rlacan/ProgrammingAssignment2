## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

################################################################################
## This function creates a special "matrix" object that can cache its inverse ##
################################################################################

makeCacheMatrix <- function(x = matrix()) {## creates function with 'x' argument of matrix type
  i <- NULL                                ## global variable 'i' initializes NULL
  set <- function(y){                      ## creates function for set other matrix with 'y' argument
    x <<- y                                ## assigns the 'y' values to the 'x' global variable 
    i <<- NULL                             ## assigns value NULL to 'i' global variable  
    print(x)                               ## prints 'x' variable 
  }
  
  get <- function() x                      ## creates function that gets the value of 'x' global variable (original matrix)
  setsolve <- function(inver) i <<- inver  ## creates function that assigns the new inverse matrix to the 'i' global variable 
  getsolve <- function() i                 ## creates function that gets the value of 'i' global variable (inverse matrix)
  list(set = set, get = get,               ## creates functions list
       setsolve = setsolve, 
       getsolve = getsolve)
  }



## Write a short comment describing this function

##################################################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above ##
##################################################################################################

cacheSolve <- function (list,...){            ## creates function with 'list' argument of list type
  inv <- list$getsolve()                      ## assigns the value of  'i' global variable to the 'inv' variable 
  if(!is.null(inv)){                          ## validates if the value of 'inv' variable it's null
    message ("getting inverse matrix cached") ## shows message
    return(inv)                               ## visualizes the value of 'inv' variable
  }
  data <- list$get()                          ## assigns the 'x' global variable to the 'data' variable
  inverse <- solve(data)                      ## calculates inverse matrix and assigns to the 'inverse' variable
  list$setsolve(inverse)                      ## assigns the inverse matrix to the 'i' global variable 
  inverse                                     ## prints the value of 'inverse' variable
}
