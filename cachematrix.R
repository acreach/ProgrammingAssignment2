## This R program simplifies the use of matrices by allowing the user to get the inverse of a matrix 
## without computing the operation every time he needs it. 

## makeCacheMatrix allows the user to create a special matrix that can contain in its additional information
## what is its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y,rows2,cols2) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(invmat) inv <<- invmat
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve is the function that computes the inverse of a matrix if it has never been done before
## for the matrix given in arguments, or just retrieve it thanks to the makeCacheMatrix function
## if it has already been calculated.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
