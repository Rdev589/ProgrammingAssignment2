## makeCacheMatrix - This function caches a matrix and its inverse
## so that the inverse can be re-used without re-calculation.  This
## function works in coordination with the below "cacheSolve" function.
##
## This function uses the "caching the mean of a vector" example
## given for this assignment as a template.

makeCacheMatrix <- function(x = matrix()) {
  ## set the invered matrix to NULL.  This will trigger
  ## the creation of the inverse of the matrix "x"
  m <- NULL
  
  ## This "set" is an important function.  This function will set the
  ## non-inverted matrix "x" in the cache and "clear" the inverse
  ## by setting it to NULL.  This "clearing" of the inverse will 
  ## subsequently trigger a cache update to re-calculate the matrix
  ## inverse.
  set <- function(y) {
    x <<- y              ## store in different envirionment
    m <<- NULL           ## store in different envirionment
  }

  ## here are some "get" and "set" functions  
  get <- function() x                 ## get the original matrix
  setinv <- function(inv) m <<- inv   ## set the inverse of the matrix
  getinv <- function() m              ## get the matrix inverse
  
  ## return list of functions defined inside of this function.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve - This function works with the cache function above
## to calculate the inverse of a matrix when necessary and store in
## the cache.  The inverse of the original matrix is returned.
##
## Assumptions:
##               1) the matrix is square
##               2) the matrix is invertible
##
## The solve function is recommended in the assignment
## one can also use the ginv function in the MASS package
##
## This function uses the "caching the mean of a vector" example
## given for this assignment as a template.
##

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {         ## if the matrix is not null then return
    message("getting cached data")
    return(m)
  }
  
  ## the matrix is not null so re-calculate the inverse
  data <- x$get()           ## first get the original non-inverted matrix
  m <- solve(data, ...)     ## now invert this matrix using solve
  x$setinv(m)               ## set the inverted matrix back into the cache
  m                         ## return the inverse of the original matrix
}


## this is a test function which also serves as a usage example for
## the above functions.
##

testMatrixCache <- function() {
  
  ## Example usage:
  ##
  C <- matrix(c(1,2,3,1), 2, 2)
  cacheC <- makeCacheMatrix(C)
  inverseC <- cacheSolve(cacheC)    ## 1st call creates the cached inverse
  inverseC2 <- cacheSolve(cacheC)   ## 2nd call is a cache hit
  ## getting cached data            ## this message is returned
  
  print("inverseC: ")
  print(inverseC)
  ## [,1] [,2]
  ## [1,] -0.2  0.6
  ## [2,]  0.4 -0.2
  
  print("inverseC2: ")
  print(inverseC2)
  ## [,1] [,2]
  ## [1,] -0.2  0.6
  ## [2,]  0.4 -0.2
  
  checkInv <- C %*% inverseC2         ## check the inverse
  ## [,1]          [,2]               ## identity matrix is returned
  ## [1,]    1 -1.110223e-16
  ## [2,]    0  1.000000e+00
  print("C %*% inverseC2: ")
  print(checkInv)
  
  print("Now reset to a new matrix to test the cache.")
  D <- matrix(c(1,2,3,1,1,2,1,3,1), 3, 3)  ## create a new matrix
  cacheC$set(D)                            ## set the new matrix, empty cache
  inverseD <- cacheSolve(cacheC)           ## re-calculates the inverse
  inverseD2 <- cacheSolve(cacheC)          ## 2nd call is a cache hit
  ## getting cached data                   ## this message is returned
  print("inverseD: ")
  print(inverseD)
  
}