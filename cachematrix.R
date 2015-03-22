## caching the inverse of a matrix

## makeCacheMatrix function makes use of the
## lexical scoping in R. A special matrix object is created that is stored in the cache
## in order to avoid unneccessary computing when requiring the inversed matrix again.
## In order, the functions do the following:

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x                                           # return the input matrix
  setInv <- function(inv) xinv <<- inv                          # set the inversed matrix
  getInv <- function() xinv                                     # return the inversed matrix
  list(set = set, get = get,  setInv = setInv,   getInv = getInv)
  
  
  
  
  
  
  
  
}
################
## CacheSolve function
###############


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##  If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve`
##  should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  mtrx <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
  if(!is.null(mtrx)) { # if the inversion result is there
    message("getting cached data")
    return(mtrx) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  mtrx <- solve(data) # we solve it
  x$setInv(mtrx) # we then set it to the object
  mtrx # return the solved result
  
  
  
        
}
