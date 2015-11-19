## ASSIGNMENT: Put comments here that give an overall description 
## of what your functions do

## The "cache matrix" function creates a special matrix object 
## that can cache its inverse. (Assignment part 1)

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) { ## First we have to set the value of the matrix. 
    x <<- y
    inv <<- NULL
    }
  get <- function() x ## Next, get the value of the matrix.
  setinverse <- function(inverse) inv <<- inverse ## Set a value for the inverse of the matrix.
  getinverse <- function() inv ## Then get a value for the inverse of the matrix.
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

## This function computes the inverse of the special matrix 
## created above. (Assignment part 2)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) { ## How? First checking to see if the inverse has been computed.
    message("getting cached data.") ## If yes, retrieve result.
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) ## If no, computes the result and sets the value in the cache.
  x$setinverse(inv)
  inv
}

## How do you test for it? First, create a matrix.
## x = rbind(c(1, 15), c(21, 6))
## m = makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
## [1,]    1   15
## [2,]   21    6

## Now solve for the cache. This is the first time around, so it will
## have to compute.

## cacheSolve(m)
##             [,1]         [,2]
## [1,] -0.01941748  0.048543689
## [2,]  0.06796117 -0.003236246

## Let's call that back a second time. Now it will draw from the cache.

## cacheSolve(m)
## getting cached data.
##             [,1]         [,2]
## [1,] -0.01941748  0.048543689
## [2,]  0.06796117 -0.003236246
