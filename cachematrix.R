
#makeCacheMatrix creates a cache for a matrix and stores it
#it takes matrix x as an argument.
#cacheSolve function takes the user input of an invertible matrix
#and assigns it to x
# m is the result required - matrix inverse

#function to cache inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inver) m <<- inver
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


#function that takes a matrix and tries to convert 
#it to an inverse using cached value if available

cacheSolve <- function(x, ...) {

  m <- x$getinv() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()   
  m <- solve(data, ...)  
  x$setinv(m)   
  m       #prints inverse of matrix
}
