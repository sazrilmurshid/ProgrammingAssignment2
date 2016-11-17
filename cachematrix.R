##The two functions works together to expedite calculating and retrieving inverse values of a matrix
##by storing the values in a cache. The functions also ensures that if an inverse value was not
##calculated, the function will calculate the inverse value to be used in the function call.

##makeCacheMatrix create a special "matrix" object that can cache its inverse.
##It also a list containing functions to 
##1. Set the value of the Matrix
##2. Get the value of the Matrix
##3. Set the inverse value of the Matrix
##4. Get the inverse value of the Matrix


makeCacheMatrix <- function(x = matrix()) {            ##set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                  ##get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse      ##set the inverse value of the matrix
  getinverse <- function() inv                         ##get the inverse value of the matrix
  list(set = set, get = get,                           ##list the functions in memory
       setinverse = setinverse,                        
       getinverse = getinverse)
}

##cacheSolve computes the inverse function of special "matrix" returned by
##makeCacheMatrix above. 
##1. It checks whether the inverse values of the matrix had been calculated
##2. Gets the inverser values from the cache and skips the computation.
##3. Calcuate the inverse vaules of the matrix if it does not exist in the cache


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                                ##Get the inverse values of the matrix
  if(!is.null(inv)) {                                  ##Check whether the inverse values are calculated 
    message("getting cached data")                     ##If inverse values are calculated display message
    return(inv)
  }
  data <- x$get()                                      ##If inverse value is not calculated, this function will be called
  inv <- inverse(data, ...)                            ##to calculate the inverse value
  x$setinverse(inv)
  inv
}