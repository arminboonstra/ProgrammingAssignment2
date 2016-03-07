## The first function calculates the inverse of the matrix and stores it.
## The second function checks if there is a stored value in the cache. If not it calculates the inverse of the matrix, otherwise it takes the value.

## This function calculates the inverse of the matrix and stores it to the cache

makeCacheMatrix <- function(x = matrix()) {

  # empty cache
  cache <- NULL
  
  # store a matrix
  cachematrix <- function(newmatrix) {
    x <<- newmatrix
    cache <<- NULL
  }
  
  # returns stored matrix
  getmatrix <- function() x
  
  # cache the inverse and get the value
  cacheinverse <- function(solve) cache <<- solve
  getinverse <- function() cache
  
  ## return the list
  list(cachematrix = cachematrix, getmatrix = getmatrix,
       cacheinverse = cacheinverse,
       getinverse = getinverse)  
  
}


## This function checks if the inverse of the matrix has already been calculated. 
## If it has it takes that value.
## Otherwise it calculates it and stores it to the cache.

cacheSolve <- function(x, ...) {
        
    ## get the cached value
    inverse <- x$getinverse()

    ## if it's there then return it
    if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    }
  
  ## otherwise cache the matrix, inverse it and store it in the cache  
  data <- x$getmatrix()
  inverse <- solve(data)
  x$cacheinverse(inverse)
  
  #return the inverse
  inverse
  
  }
