
# Two functions, makeChacheMatrix and chacheSolve, that cache the inverse of a matrix

# The makeCacheMatrix is for creating a special matrix object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL # initialize inverse as NULL
  
  #set matrix
  set <- function(y){
    m<<-y
    inv <<- NULL
  }
  
  #get and return matrix
  get<-function() m
  
  #set inverse
  setInverse <- function(inverse) inv<<-inverse
  
  #get and return inverse
  getInverse <- function() inv
  
  #returns list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# The cacheSolve computes the inverse of the special matrix by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #return the stored inverse
  
  #return the inverse if it already has been set
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data) %*% data #calculates the inverse value
  x$setInverse(inv)
  
  inv # Returns the inverse
}

