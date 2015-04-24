##makeCacheMatrix creates a special "matrix" object that can cache its inverse
##cacheSolve checks to see if the matrix it receives has already had its inverse
##computed and cached by makeCacheMatrix. If so, it retrieves it.
##If not, cacheSolve proceeds to compute the inverse of the matrix and passes it to
##makeCacheMatrix for storage

##Set the initial value of the inverse to NULL and create an object "x" that contains
##a list of functions for getting and setting the matrix and its inverse
##cacheSolve will use subsets of the list to access these functions

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Check to see if the inverse of the matrix has been stored by makeCacheMatrix. 
##If the inverse has been stored (and the matrix has not changed), 
##retrieve it, print "getting cached data" and return the value
##If the inverse has not been stored, calculate it, pass it
##to makeCacheMatrix for storage, and return the value

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
