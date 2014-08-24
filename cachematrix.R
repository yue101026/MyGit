## date:2014-8-23
## Cathy Liu

## This function creates a special "matrix" object that can cache its inverse.
## You can create the matrix as follows
## mat <- makeCacheMatrix(diag(5,5))
## mat$get() returns the matrix itself.
## mat$set(diag(6,5)) can change the matrix to another one.
## mat$getInverseMatrix() can get the inversed matrix if it exists.

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse_matrix) InverseMatrix <<- inverse_matrix
  getInverseMatrix <- function() InverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

## After you have created a special matrix by mat <- makeCacheMatrix(diag(5,5))
## You can calculate the inversed matrix by
## inmat<- cacheSolve(mat)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'        
  InverseMatrix <- x$getInverseMatrix()
  if(!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  data <- x$get()
  InverseMatrix <- solve(data, ...)
  x$setInverseMatrix(InverseMatrix)
  InverseMatrix
  
}
