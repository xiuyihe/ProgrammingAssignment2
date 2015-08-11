##the two functions get the inverse matrix of one matrix x, then store it 
##in a different place, in order to save the time calculating it next time
##if the matrix changes, the second function will calculate and return the 
##right inverse matrix


## the makeCacheMatrix function get the inverse matrix of x
## then store the inverse matrix in a different environment

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinversematrix <- function(solve) m <<- solve
      getinversematrix <- function() m
      list(set = set, get = get,
           setinversematrix = setinversematrix,
           getinversematrix = getinversematrix)
}


## if the matrix x is the same, return the stored inverse matrix
## if not, calculate and return the correct inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinversematrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinversematrix(m)
      m
}
