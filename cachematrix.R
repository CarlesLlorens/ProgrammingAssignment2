## Functions that computes the inverse of a matrix
## Don't recompute if prevously obtained the inverse (get from cache)

## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(mat=matrix()) {
  
  # Initialize 
  inv <- NULL
  
  # Set method
  set <- function(s_mat){
    mat <<- s_mat
    inv <<- NULL
  }
  
  # Get method
  get <- function() {
    mat       # Return the matrix
  }
  
  setInverse<-function(solve) inv<<- solve
  getInverse<-function() inv
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## This function computes inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mat=matrix(), ...) {
  
  ## Return the inverse of parameter "mat"
  inv<-mat$getInverse()
  
  ## If the Inverse exists in cache, return it without recompute
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  matrix<-mat$get()       # Get the matrix from object
  inv<-solve(matrix, ...) # Compute the Inverse
  mat$setInverse(inv)     # Set the inverse 
  inv                     # Return de Inverse matrix
}
## Test: 
## Computes the Inverse of matrix(1:4,2,2)
## x <- matrix(1:4,2,2)
## a <- makeCacheMatrix()
## a$set(x)
## inv_x<-cacheSolve(a)
## ... some code
## inv_x<-cacheSolve(a) 
## Appears "getting cached data" and Inverse is NOT recompute

##
## Note: x * inverse(x) = Identity  
## x %*% inv_x 
## must give an Identity matrix
