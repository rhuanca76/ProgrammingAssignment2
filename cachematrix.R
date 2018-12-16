# general approach is:
# - define a custom matrix object to handle inverse
# - define an special function that calculates and caches matrix inverse

# makeCacheMatrix does:
# 1. CacheMatrix object has 4 functions
#    - set() sets a value in CacheMatrix object
#    - get() retrieves the value stored CacheMatrix object
#    - setinverse() sets the value of the cached inverse value
#    - getinverse() retrieves the cached inverse value
#    - i var is the cached inverse value
makeCacheMatrix <- function(x = matrix()) {
  i = NULL
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

# cacheSolve arguments:
# x: the CacheMatrix object
# ...: optional arguments that can be passed to 
#      solve function along with CacheMatrix' data
# cacheSolve does:
# 1. calls getinverse function of CacheMatrix object
# 2. in case inverse is not defined it is calculated 
#    calling R's solve function and after that is being cached

cacheSolve <- function(x, ...) {
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
