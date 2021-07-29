## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse matrix
## get the inverse matrix

##Usage
# x <- matrix(c(1,0,1,1),2,2)   #source matrix
# m <- makeCacheMatrix(x)
# y <- m$get()
# m$setinverse(solve(x))
# invx <- m$getinverse()

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL

  #set source matrix  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get source matrix
  get <- function() x
  
  #set inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #get inverse matrix 
  getinverse <- function() inv
  
  #return the list object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setinverse function.

##Usage
# cacheSolve(m) 


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
#compute inverse matrix if not in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
