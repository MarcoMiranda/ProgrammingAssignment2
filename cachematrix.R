# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #sets the inv variable to null
  set <- function(y) {  # function to set vector x to a new vertor y and resets inv to null
    x <<- y
    inv <<- NULL        
  }
  get <- function() x   #returns x
  setinverse <- function(inverse) inv <<- inverse #asigns inverse to inv
  getinverse <- function() inv    #returns inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #returns a list of components
}

# cacheSolve function returns the inverse of the matrix eather from cache or calculated. It first checks if
# the inverse has already been computed. If in the cache it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()     #asigns the getinverse to inv
  if(!is.null(inv)) {       #tests to see if there is an inv in the cache
    message("getting cached data.")   #lets you know there is an inv value cacheded
    return(inv)
  }
  data <- x$get()         #asigns the x output to data
  inv <- solve(data)      #calculates the inverse of data thru the solve function
  x$setinverse(inv)       #asigns the inv  to the cache
  inv                     #returns the inverse 
}