## This is the code for Assignment 1 where we have to cache the inverse of the matrix and retrieve from the cache when the matrix for which we need to calculate the inverse is same

## Function to initialize the cache matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to determine the inverse of the matrix. Here it first checks the cache to see if inverse of the passed matric is stored in cache or not. It evaluates 
##inverse only if the cache is empty

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

##Testing the functionality
## Create a square matrix of 2*2 dimesion

f = makeCacheMatrix(matrix(c(1,0,0,1),2,2))

#Determine the inverse
cacheSolve(f)

#Determine the inverse again and we can see the code pulls the data from cache
cacheSolve(f)
