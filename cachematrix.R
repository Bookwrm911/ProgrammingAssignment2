## In tandem, makeCaheMAtrix and cacheSolveare designed to streamline the calulation of the inverse of a given matrix.
##makeCaheMatrix helps to create the matrix, and inverses, while cacheSolve helps to minimize the duplication of computation, 
##by drawing from the cache previous calculations, when appropriate.

makeCacheMatrix <- function(x = matrix()) {
  ##makeCacheMatrix is designed to make a invertible matrix.
  ##Will be used for cacheSolve later as a input as a cache that can be drawn from. 
  
    inv <- NULL
  set <- function(y) { ##Sets the info for the matirix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##Gets the Desired matrix from the above data.
  set.inv <- function(inverse) inv <<- inverse #Takes the above data from the matrix and sets its inverse.
  get.inv <- function() inv ##Used the above to create the new inverted matrix.
  list(set=set, get=get, set.inv=set.inv, get.inv=get.inv) ##Returns the new matrix, as per new dimensions.
}


## The following function returns the inverse of the inputted matrix.
##First, it checks to see if the desired inverse had been computed previously.
##If so, it will simply utilize the pre-existing one.
##If not, it will calucate the new inverse and cache it.

cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  
  if (!is.null(inv)) {  ## Examines to see if inverse already calculated.
    message("Getting Cached Data. Please Stand by.")##If it exists, the previous calculation is returned
    return(inv)
  }
  
  # If the inverse if NOT calculated yet, calulate it!
  data <- x$get()
  inv <- solve(data, ...)
  
  # Place the calulate in the cache.
  x$setinv(inv)
  
  # Return the calculated inverse.
  inv
}