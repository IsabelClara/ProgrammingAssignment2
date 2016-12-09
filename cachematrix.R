## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #when this function is called,  
    x <<- y #the argument is reset,
    inv <<- NULL # and then the value of the inverse is set to Null (it would be necessary to call cacheSolve to calculate it again)
  }
  get <- function() x #gives the matrix
  setinv <- function(inverse) inv <<- inverse #set a new value for the inverse of the matrix
  getinv <- function() inv #get the inverse of the matrix calculated by cacheSolve, if we haven't call cacheMatrix yet, it will return NULL
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #calculates the inverse of the matrix given by makeCacheMatrix
  inv <- x$getinv()  #gets the value 
  if(!is.null(inv)) { #if the value is not NULL, the function takes it from the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   #if inv contains a NULL value, cacheSolve calculates it
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
