## function:    makeCacheMatrix 
## description: This function will hold getters and setters for storing a matrix and its corresponding inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  storedInverseMatrix <- NULL
  
  # function:     set
  # description:  stores the input parameter into the local variable 'x'. Assumes 'x' is a square matrix. Also resets
  #               the corresponding inverted matrix to null.
  set <- function(newMatrix) {
    if (!identical(newMatrix,x)) {
      x <<- newMatrix
      storedInverseMatrix <<- NULL
    }
  }
  
  # function:     get
  # description:  returns the matrix stored in local variable 'x'.
  get <- function() {
    return(x)
  }
  
  # function:     setInverseMatrix
  # description:  stores the input parameter into the local variable 'storedInverseMatrix'. Assumes 'newInverseMatrix'
  #               is a square matrix and is the inverse matrix for 'x'.
  setInverseMatrix <- function(newInverseMatrix) {
    storedInverseMatrix <<- newInverseMatrix
  }
  
  # function:     getInverseMatrix
  # description:  returns the matrix stored in local variable 'storedInverseMatrix'
  getInverseMatrix <- function () {
    return(storedInverseMatrix)
  }
  
  list(set=set, get=get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## function:    cacheSolve 
## description: This function will accept a list of type makeCacheMatrix and will look in the 
##              environment to see if the cache has an inverse matrix stored for that object. If
##              there is an inverse matrix then this function will return it to the caller. If there
##              is no inverse matrix then this function will calculate the inverse matrix, store it and return
#               it to the caller.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  tempInverse <- x$getInverseMatrix()
  
  #1) Check to see if a value was returned. If so, return that value to the caller.
  if(!is.null(tempInverse)) {
    message("getting cached inverseMatrix")
    return(tempInverse)
  }
  
  #2) Assert - no value has been returned. Call the solve() function to determine the inverse value for
  #            the matrix stored in the object that was passed to this function.
  data <- x$get()
  tempInverse <- solve(data)
  
  #3) Store the inverse matrix in the object
  x$setInverseMatrix(tempInverse)
  
  #4) Return the inverted matrix to the caller
  message("calculated cached invertMatrix")
  return(tempInverse)
}


