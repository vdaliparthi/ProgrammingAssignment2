## makeCacheMatrix function creates a special "matrix", which is really a list containing functions to
## 1) set a new matrix data in the cache, 2) get the matrix data from the cache, 
## 3) set the inverse value of matirx into cache and 
## 4) get the inverse matrix value from the cache
##
## cacheSolve function calculates/solves the inverse matrix for the special matrix made using makeCacheMatrix  
## It first checks to see if the inverse of the matrix has already been solved. 
## If so, it gets the inverse matrix from the cache and skips the re-calculation. 
## Otherwise, it calculates the inverse of the matrix and 
## updates the value in the cache via the cacheInvMatrix function.


## This function creates a special "matrix" object that can cache its inverse.
## It takes one argument of matrix class 
makeCacheMatrix <- function(x = matrix()) {
 
  InvMatrix <- NULL   #Reset the value of inverse matrix
  
  setMatrix <- function(y) {
    x <<- y             # Set the matrix data x with new data y 
    InvMatrix <<- NULL  # Reset the value of inverse matrix
  }
  
  getMatrix <- function() x  #Return the matrix data from cache
  
  cacheInvMatrix <- function(z) InvMatrix <<- z   #push the inverse matirx into cache
  
  getCachedInvMatrix <- function() InvMatrix    #Return cached inverse matrix data
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, 
       cacheInvMatrix=cacheInvMatrix, 
       getCachedInvMatrix=getCachedInvMatrix)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve would return the inverse matrix from the cache.
## Argument x is the special "matrix" returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  
  iMat <- x$getCachedInvMatrix() #Retrive the inverse matrix of x from the cache if any
  
  if (!is.null(iMat)){  #If chache value is not emply
    
    message("Getting Inversed Matrix from cached data")
    return(iMat) #Return the cached inverse matrix
    
  }
  
  m <- x$getMatrix()  #When chached inverse value is empty get the matrix from cache to solve it fresh 
  
  iMat <- x$cacheInvMatrix(solve(m)) #Solve the inverse of the matrix and cache it for next use
  
  return(iMat) #Return the cached inverse matrix
}