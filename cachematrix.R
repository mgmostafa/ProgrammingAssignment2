##################################################################################
## This R code contains two functions: makeCacheMatrix and cacheSolve. The first
## caches the inverse of a square matrix. While the later find the inverse of the
## cached matrix if it was not cached already.
#################################################################################


## The makeCacheMatrix function cashes the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
     im <- NULL
     set <- function(y) {
          x <<- y
          im <<- NULL
     }
     get <- function() x
     setinv <- function(mat) im <- solve(mat)
     getinv <- function() im 
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function find the inverse of a matrix if it was not cached 
## by the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x' (if cached)
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## Compute and Return a matrix that is the inverse of 'x' (not cached)
     mat <- x$get()
     m <- solve(mat, ...)
     x$setinv(m)
     m
}

# Test Case
# x <- matrix(1:4, 2, 2)
# mx <- makeCacheMatrix(x)
# ix <- cacheSolve(mx)
# ix %*% x
