## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(base)
 makeCacheMatrix <- function(x = matrix())        
   {         
  inv <- NULL               #creating a cache matrix function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x         
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
    inver <- ginv(x)
    inver%*%x                      #inverse matrix function
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
 #function returning inverse of above matrix
cacheSolve <- function(x, ...) {  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  
  x$setinv(inv)
  inv
}
       '## Return a matrix that is the inverse of x
