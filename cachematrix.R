## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     im <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
          im <<-NULL
     }
     get <- function() x
     setmat <- function(mat) m <<- mat
     getmat <- function() m
     setinv <- function(mat) im <- solve(mat)
     getinv <- function() im 
     list(set = set, get = get,
          setmat = setmat,
          getmat = getmat,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getmat()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     mat <- x$get()
     m <- solve(mat, ...)
     x$setmat(m)
     m
}

# Test Case
# x <- matrix(1:4, 2, 2)
# mx <- makeCacheMatrix(x)
# ix <- cacheSolve(mx)
# ix %*% x
