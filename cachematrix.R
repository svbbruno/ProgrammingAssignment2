## makeCacheMatrix will define a list of functions to manipule a matrix
## and cache its inverse. cacheSolve computes an inverse, if not available in the cache
# both functions are based in the vector examples from 
#https://class.coursera.org/rprog-010/human_grading/view/
#courses/973491/assessments/3/submissions

#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#- set the value of the matrix
#- get the value of the matrix
#- set the value of the inverse
#- get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, 
# it gets the inverse from the cache and skips the computation. Otherwise, it calculates
#the inverse of the data and sets the value of the inverse in the cache via the
#setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
