## These two functions cache the inverse of a matrix.  Matrix inversion can be a costly
## computation so saving the result in a cache may be more optimal to computing it repeatedly.  
## We make use of the superassignment operator <<- as per the assignment rubric. 

## makeCacheMatrix() is the matrix analog to the makeVector() function provided, 
## thus it takes a matrix paramater. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() returns a matrix that is the inverse of 'x'.  If the inverse has already been 
## cached,it returns that; otherwise the inverse is calculated using the solve() function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
