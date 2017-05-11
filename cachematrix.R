## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to

      ## set the value of the matrix
      ## get the value of the matrix
      ## set the value of the Inverse
      ## get the value of the Inverse


makeCacheMatrix <- function(x = matrix()) {

    inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinver <- function(inverse) inver <<- inverse
    getinver <- function() inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)

}

## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix of data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matri <- x$get()
  inver <- inverse(matri, ...)
  x$setinverse(inver)
  inver
  
  }
