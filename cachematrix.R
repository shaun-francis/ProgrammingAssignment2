## Put comments here that give an overall description of what your
## functions do
## These pair of functions cache the inverse of a matrix
## This is to save time lost in computation

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL  ##This will hold the value of inverse
  set<- function(y)  ##define the set function to assign new value
  {
    x<<- y   ## value of matrix in parent environment
    inv<<- NULL ## if there is a new matrix, inv needs to be set to NULL
  }
  get <- function() x ## define the get function - returns value of the matrix
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## in order to use $ operator
}


## Write a short comment describing this function
## Computes inverse of special "matrix" returned from above function
## if no change in matrix and inverse has been calculated it retrives data from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)) ## checking if inverse exists
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
