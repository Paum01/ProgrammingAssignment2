## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  storedInverseMatrix <- NULL
  
  set <- function(y) {
    x <- y
    storedInverseMatrix <<- NULL
  }
  
  
  get <- function() {
    x
  }
  
  setInverseMatrix <- function(newInverseMatrix) {
    storedInverseMatrix <<- newInverseMatrix
  }
  
  getInverseMatrix <- function () {
    return(storedInverseMatrix)
  }
  
  list(set=set, get=get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  
  if(!is.null(m)) {
    message("getting cached inverseMatrix")
    return(m)
  }
  
  data <- x$getMatrix()
  m <- solve(data)
  
  x$setInverseMatrix(m)
}
