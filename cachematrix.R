## Put comments here that give an overall description of what your
## functions do

#these are two functions that work in tandem
#they allow to store a square matrix along with it inverse (makeCacheMatrix)
#and to solve the matrix or retrieve the solution if it is already stored (cacheSolve)

## Write a short comment describing this function

#makeCacheMatrix creates a list with a matrix and methods: 
#
#set: to store the matrix value
#get: to retrieve the matrix value
#setinverse: stores the inverse of the square matrix
#getinverse: retrieves the inverse of the square matrix


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

#cacheSolve firte checks wether the inverse is already stored
#if it's note, solves the matrix and stores it using makeCacheMatrix
#in both cases returns the value of the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
