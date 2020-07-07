## Put comments here that give an overall description of what your
## functions do
##Caching the Inverse of a Matrix:
  ## Matrix inversion is usually a costly computation and there may be some 
  ## benefit to caching the inverse of a matrix rather than compute it repeatedly.
  ## Here  are a pair of functions that are used to create a special object that 
  ## stores a matrix and caches its inverse.

## Write a short comment describing this function
##This is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix()  above. If the inverse has already been calculated and the 
## matrix is same, then it will  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


M <- matrix(c(1,2,3,4),2,2)
M
M1<- makeCacheMatrix(B)
cacheSolve(M1)  #inverse returned after computation
cacheSolve(M1) #inverse returned from cache

M1$set( M1$ getinverse()) #Inverse of the inverse is the original matrix
cacheSolve(M1)
