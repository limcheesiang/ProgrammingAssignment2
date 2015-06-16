## Put comments here that give an overall description of what your
## functions do


# ##################################
# Test cases #1
# > m <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T)
# > cm <- makeCacheMatrix(m)
# > cacheSolve(cm)
# calculating and caching inverse
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > cacheSolve(cm)
# getting cached results
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
#
# ##################################
# Test case #2
# > m <- matrix(c(1,0,4, 1,3,4, 4,1,0), nrow=3, ncol=3, byrow=T)
# > cm <- makeCacheMatrix(m)
# > cacheSolve(cm)
# calculating and caching inverse
# [,1]        [,2]    [,3]
# [1,]  0.08333333 -0.08333333  0.2500
# [2,] -0.33333333  0.33333333  0.0000
# [3,]  0.22916667  0.02083333 -0.0625
# > cacheSolve(cm)
# getting cached results
# [,1]        [,2]    [,3]
# [1,]  0.08333333 -0.08333333  0.2500
# [2,] -0.33333333  0.33333333  0.0000
# [3,]  0.22916667  0.02083333 -0.0625
# 
# ##################################
# Test case #3
# > set.seed(100)
# > rv <- rnorm(4*4,10)
# > m <- matrix(rv, nrow=4, ncol=4)
# > cm<-makeCacheMatrix(m)
# > cacheSolve(cm)
# calculating and caching inverse
# [,1]       [,2]        [,3]       [,4]
# [1,] -2.1351004  1.3615090 -0.53674893  1.1766314
# [2,]  1.5171286 -0.8672346 -0.38722162 -0.1636225
# [3,]  1.2559340 -1.7098003  1.07654490 -0.4855626
# [4,] -0.5707933  1.1766667 -0.08793017 -0.5169376
# > cacheSolve(cm)
# getting cached results
# [,1]       [,2]        [,3]       [,4]
# [1,] -2.1351004  1.3615090 -0.53674893  1.1766314
# [2,]  1.5171286 -0.8672346 -0.38722162 -0.1636225
# [3,]  1.2559340 -1.7098003  1.07654490 -0.4855626
# [4,] -0.5707933  1.1766667 -0.08793017 -0.5169376
# 

## Write a short comment describing this function
# Given a regular matrix, makeCacheMatrix create a special matrix
# which is a list containing functions to 
# - set the input matrix
# - get the cache matrix
# - set the solved inverse
# - get the cache inverse
##
makeCacheMatrix <- function(mat = matrix()) {
 mInv <- NULL
 set <- function(val) {
   mat <<- val
   mInv <<- NULL
 }
 get <- function() mat
 setInverse <- function(inv) mInv <<- inv
 getInverse <- function() mInv
 list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}

## Write a short comment describing this function
# Input to cacheSolve is a special matrix created by makeCacheMatrix.
# Firstly, cacheSolve retrieves the inverse. If it's present,
# it returns the cached inverse. Otherwise, it retrieves the regular
# matrix, solves its inverse and caches the results.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached results")
    return(inv)
  }
  data <- x$get()
  message("calculating and caching inverse")
  invs <- solve(data, ...)
  x$setInverse(invs)
  invs
}
