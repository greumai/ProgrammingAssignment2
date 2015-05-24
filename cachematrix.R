## The first function, makeCacheMatrix, creates a special matrix, which is really a list containing a function to
##        set the value of the matrix
##        get the value of the vector
##        set the value of the inversed matrix
##        get the value of the inversed matrix

makeCacheMatrix <- function(mat = matrix()) {
      inv <- NULL
      set <- function(value) {
              mat <<- value
              inv <<- NULL
      }
      get <- function() mat
      setInversed <- function(inversedMat) inv <<- inversedMat
      getInversed <- function() inv
      list(set = set, get = get,
              setInversed = setInversed,
              getInversed = getInversed)
}


## The second function, cacheSolve, computes the inversed matrix, but only if the computation has not been already done and cached.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInversed()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInversed(inv)
      inv
}
