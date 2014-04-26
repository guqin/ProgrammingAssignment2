

## makeCacheMatrix function
## The CacheMatrix object creator, makeCacheMatrix, that contains accessors
## for the original matrix x (get) and to get and set it's 
## inverse (setmatrixinverse, getmatrixinverse).  
## getmatrixinverse returns NULL if it has
## not been set to the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(matrixinverse) m <<- matrixinverse
  getmatrixinverse <- function() m
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
  
}


## cacheSolve function
## If the CacheMatrix object has a computed inverse 
## return it from the cache.  Else compute it, save it
## in the cache and return it
## it 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinverse(m)
  m
}


